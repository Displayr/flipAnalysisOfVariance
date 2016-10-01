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
#' @param correction The multiple comparison adjustment method: \code{"Tukey Range", "None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"}, \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} \code{"Shaffer"}, and \code{"Westfall"}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance. This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param missing How missing data is to be treated in the ANOVA. Options:
#'   \code{"Error if missing data"}, and
#'   \code{"Exclude cases with missing data"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param p.cutoff The alpha level to be used in testing.
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param binary Automatically converts non-ordered factors to dummy-coded (binary indicator) variables.
#' @param ... Other parameters to be passed to \code{OneWayANOVA}.
#' @details Where sampling weights are provided, a sample is constructed via bootstrapping, and this
#' sample is used in the testing for the MANOVA (but no for the individual ANOVAs or post hoc tests).
#' See \code{OneWayANOVA} for more about the other parameters.
#'
#' Tests are two-sided, comparing to the Grand Mean (i.e., "To mean").
#'
#' Additional detail about the other parameters can be found in \code{OneWayANOVA}.
#' @importFrom flipRegression Regression GrandMean
#' @importFrom flipTransformations AsNumeric Factor
#' @importFrom flipData CheckForLinearDependence
#' @importFrom multcomp glht mcp adjusted
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable OriginalName
#' @importFrom survey regTermTest
#' @importFrom flipData SampleDescription CalibrateWeight
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @importFrom stats complete.cases lm manova
#' @export
OneWayMANOVA <- function(outcomes,
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        correction = "Tukey Range",
                        robust.se = FALSE,
                        missing = "Exclude cases with missing data",
                        show.labels = FALSE,
                        seed = 1223,
                        p.cutoff = 0.05,
                        binary = FALSE,
                        ...)
{
    # Removing missing values and filtering weights.
    predictor.label <- if(show.labels) Labels(predictor) else OriginalName(predictor)
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
    outcomes <- AsNumeric(data.frame(outcomes), binary = binary, remove.first = TRUE)
    labels <- Labels(outcomes)
    df <- cbind(outcomes, predictor, weights)
    df <- subset(df, subset = subset & complete.cases(df))
    n.estimation <- nrow(df)
    if (n.estimation < n.total & missing == "Error if missing data")
        stop("Data contains missing values.")
    n.variables <- ncol(outcomes)
    predictor <- df[, n.variables + 1]
    outcomes <- df[, 1:n.variables]

    footer <- SampleDescription(n.total, n.subset, n.estimation, subset.label, weighted, weight.label, missing = "", imputation.label = NULL, NULL)
    if (weighted)
    {
        wgt <- CalibrateWeight(df[, n.variables + 2])
        df <- AdjustDataToReflectWeights(df[, 1:(n.variables + 1)], wgt)
        o.matrix <- as.matrix(df[, 1:n.variables])
        CheckForLinearDependence(o.matrix)
        g <- df[, n.variables + 1]
        g <- removeMissingLevels(g)
        footer <- paste0(footer,
                         "; Pillau's trace computed using a resampled sample of ",
                         nrow(df),
                         " observations (the effective sample is ", nrow(df),
                         ")")
        model <- lm(o.matrix ~ g)
    }
    else
    {
        o.matrix <- as.matrix(outcomes)
        CheckForLinearDependence(o.matrix)
        predictor <- removeMissingLevels(predictor)
        model <- lm(o.matrix ~ predictor)
    }
    result <- list(manova = summary(manova(model)))
    result$anovas <- MultipleANOVAs( outcomes,
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        compare = "To mean",
                        correction = correction,
                        alernative = "Two-sided",
                        show.labels = show.labels,
                        p.cutoff = p.cutoff,
                        seed = seed,
                        ...)
    if (show.labels)
        names(result$anovas) <- labels
    class(result) <- "OneWayMANOVA"
    result$title <- paste0("MANOVA: ", predictor.label)
    result$p <- p <- result$manova$stats[1,6]
    result$pillai <- pillai <- result$manova$stats[1,2]
    result$subtitle <- paste0(if (p <= p.cutoff) "Significant" else "Not significant",
                              ": Pillai's Trace: ",
                              FormatAsReal(pillai, 3),
                              ", approximate p-value: ",
                              FormatAsPValue(p))
    result
}


#' removeMissingLevels
#' @param x A factor
removeMissingLevels <- function(x)
{
    tbl <- table(x)
    if (min(tbl) == 0)
    {
        label <- Labels(x)
        print(label)
        stop("dog")
        warning(paste0("One or more categories of ", label, " do not appear in the data: ", names(tbl[tbl == 0]),
                       ". This may be because they are empty in the raw data, or because they are empty after any weights, filters/subsets, or missing data settings are applied. ",
                        "This may cause an error. It is recommended that you merge categories prior to estimating the model, use an alternative missing data method, filter the data, or make the data numeric."))
        x <- Factor(x)
    }
    x
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
    print(FormattableANOVAs(x$anovas,
        title = x$title,
        subtitle = x$subtitle,
        footer = paste0(x$anovas[[1]]$footer, " (corrections performed within rows)")))
}

