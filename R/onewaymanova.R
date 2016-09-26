#' \code{OneWayMANOVA}
#'
#' Computes a one-way analysis of variance with post hoc tests.
#' @param outcomes The outcome variables.
#' @param predictor The factor representing the groups.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param compare One of \code{"To mean", "Pairwise", "To first"} (which implement's Dunnett's C, when
#' combined with 'correction' == 'Tukey Range'), or \code{"All"}
#' @param correction The multiple comparison adjustment method: \code{"Tukey Range", "None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"}, \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} \code{"Shaffer"}, and \code{"Westfall"}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance. This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param alternative The alternative hypothesis: "Two sided", "Greater", or "Less". The main application of this is when
#' Compare us set 'To first' (e.g., if testing a new product, where the purpose is to work out of the new product is superior
#' to an existing product, "Greater" would be chosen).
#' @param missing How missing data is to be treated in the ANOVA. Options:
#'   \code{"Error if missing data"}.
#'   \code{"Exclude cases with missing data"}, and
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param p.cutoff The alpha level to be used in testing.
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param binary Automatically converts non-ordered factors to dummy-coded (binary indicator) variables.
#' @param ... Other parameters to be passed to \code{OneWayANOVA}.
#' @details Where sampling weights are provided, a sample is constructed via bootstrapping, and this
#' sample is used in the testing for the MANOVA (but no for the individual ANOVAs or post hoc tests). See \code{OneWayANOVA} for more about the other parameters.
#'
#' Additional detail about the other parameters can be found in \code{OneWayANOVA}.
#' @importFrom flipRegression Regression GrandMean
#' @importFrom flipTransformations AsNumeric Factor
#' @importFrom multcomp glht mcp adjusted
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable
#' @importFrom survey regTermTest
#' @importFrom flipData SampleDescription CalibrateWeight
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @importFrom stats complete.cases lm manova
#' @export
OneWayMANOVA <- function(outcomes,
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        compare = "To mean",
                        correction = "Tukey Range",
                        robust.se = FALSE,
                        alternative = "Two-sided",
                        missing = "Exclude cases with missing data",
                        show.labels = FALSE,
                        seed = 1223,
                        p.cutoff = 0.05,
                        binary = FALSE,
                        ...)
{
    # Removing missing values and filtering weights.
    n.total <- length(predictor)
    weighted <- !is.null(weights)
    if (!weighted)
        weights <- rep(1, n.total)
    else
        weight.label <- Labels(weights)
    if (is.null(subset) | length(subset) == 1)
        subset <- rep(TRUE, n.total)
    else
        subset.label <- Labels(subset)
    n.subset <- sum(subset)
    outcomes <- AsNumeric(outcomes, binary = binary, remove.first = TRUE)
    labels <- Labels(outcomes)
    predictor.label <- Labels(predictor)
    df <- cbind(outcomes, predictor, weights)
    df <- subset(df, subset = subset & complete.cases(df))
    n.estimation <- nrow(df)
    n.variables <- ncol(outcomes)
    predictor <- df[, n.variables + 1]
    outcomes <- df[, 1:n.variables]
    footer <- SampleDescription(n.total, n.subset, n.estimation, subset.label, weighted, weight.label, missing = "", imputation.label = NULL, NULL)
    if (weighted)
    {
        wgt <- CalibrateWeight(df[, n.variables + 2])
        df <- AdjustDataToReflectWeights(df[, 1:(n.variables + 1)], wgt)
        o.matrix <- as.matrix(df[, -1:-n.variables])
        g <- df[, n.variables + 1]
        footer <- paste0(footer,
                         "; Pillau's trace computed using a resampled sample of ",
                         nrow(df),
                         " observations (the effective sample is ", nrow(df),
                         ")")
        model <- lm(o.matrix ~ g)
    }
    else
        model <- lm(as.matrix(outcomes) ~ predictor)
    result <- list(manova = summary(manova(model)))
    result$anovas <- MultipleANOVAs(outcomes = outcomes,
                        predictor = predictor,
                        subset = NULL,
                        weights = NULL,
                        compare = compare,
                        correction = correction,
                        show.labels = show.labels,
                        p.cutoff = p.cutoff,
                        ...)
    result$predictor.label <- predictor.label
    class(result) <- "OneWayMANOVA"
    result
    }

#' print.OneWayMANOVA
#'
#' Returns the OneWayMANOVA as a pretty table.
#' @param x A OneWayMANOVA object.
#' @param ... Other arguments.
#' @importFrom flipFormat FormatAsReal FormatAsPValue
#' @export
print.OneWayMANOVA <- function(x, ...)
{
    PrintFormattableANOVAs(x$anovas,
        title = paste0("MANOVA: ",x$predictor.label),
        subtitle = paste0("Pillai's Trace: ", FormatAsReal(x$manova$stats[1,2], 3), ", approximate p-value: ", FormatAsPValue(x$manova$stats[1,6])),
        footer = paste0(x$anovas[[1]]$footer, " (corrections performed within rows)"))

}

#' PrintFormattableANOVAs
#'
#' @param anovas List of OneWayANOVA objects.
#' @param title String showing the title of the table.
#' @param subtitle String showing the subtitle of the table.
#' @param footer String showing the title of the table.
#' @importFrom flipFormat MeanComparisonsTable
#' @export
PrintFormattableANOVAs <- function(anovas, title, subtitle, footer)
{
    mt <- ANOVAsAsTable(anovas)
    mct <- MeanComparisonsTable(
        means = mt$means,
        zs = mt$zs,
        ps = mt$ps,
        r.squared = mt$r.squared,
        overall.p = mt$overall.p,
        column.names = mt$column.names,
        title = title,
        subtitle = subtitle,
        footer = footer)
    print(mct)
    invisible(anovas)
}

#' \code{ANOVAsAsTable}
#' Converts a list of ANOVAs into a format that can be prettily formatted.
#' @param x The list of ANOVAs.
#' @export
ANOVAsAsTable <- function(x)
{
    means <- NULL
    zs <- NULL
    ps <- NULL
    r.squared <- NULL
    overall.p <- NULL
    for (i in x)
    {
        coefs <- i$coefs
        means <- rbind(means, coefs[, 1])
        zs <- rbind(zs, coefs[, 3])
        ps <- rbind(ps, coefs[, 4])
        r.squared <- c(r.squared, i$r.squared)
        overall.p <- c(overall.p, i$p)
    }
    rownames(means) <- names(x)
    column.names <- paste0(x[[1]]$column.names, "<br>","n = ",x[[1]]$n)
    colnames(means) <- LETTERS[1:(k <- ncol(means))]
    colnames(ps) <- paste0(LETTERS[1:k], "1")
    return(list(means = means,
                zs = zs,
                ps = ps,
                r.squared = r.squared,
                overall.p = overall.p,
                column.names = column.names))
}

