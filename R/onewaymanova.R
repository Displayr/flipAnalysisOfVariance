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
#' @param pillai If \code{TRUE}, Pillai's Trace is computed as the overall MANOVA statistic.
#' @param fdr If \code{TRUE}, the False Discovery Rate correction is applied. This is the default.
#' @param ... Other parameters to be passed to \code{OneWayANOVA}.
#' @details By default, the overall p-value is computed as the smallest p-value in any cell following application of the
#' False Discovery Rate correction to the p-values. If the\code{fdr} is set to \code{FALSE}, the correction is not applied, which means
#' that the overall p-value is the smallest of the uncorrected p-values, and, additionally, the p-values for each row
#' are from the \link{OneWayANOVA} F-tests.
#'
#' Tests are two-sided, comparing to the Grand Mean (i.e., "To mean" in \link{OneWayANOVA}).
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
#' @importFrom stats complete.cases lm manova p.adjust
#' @export
OneWayMANOVA <- function(outcomes,
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        robust.se = FALSE,
                        missing = "Exclude cases with missing data",
                        show.labels = FALSE,
                        seed = 1223,
                        p.cutoff = 0.05,
                        binary = FALSE,
                        pillai = FALSE,
                        fdr = TRUE,
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
    footer <- paste0(footer, "two-sided comparisons against the row means", if (fdr) " (p-values corrected using False Discovery Rate)")
    if (weighted)
    {
        wgt <- CalibrateWeight(df[, n.variables + 2])
        df <- AdjustDataToReflectWeights(df[, 1:(n.variables + 1)], wgt)
        o.matrix <- as.matrix(df[, 1:n.variables])
        CheckForLinearDependence(o.matrix)
        g <- df[, n.variables + 1]
        g <- removeMissingLevels(g)
        if (pillai)
            stop("Pillai's trace cannot be correctly computed with weighted data.")
        model <- lm(o.matrix ~ g)
    }
    else
    {
        o.matrix <- as.matrix(outcomes)
        CheckForLinearDependence(o.matrix)
        predictor <- removeMissingLevels(predictor)
        model <- lm(o.matrix ~ predictor)
    }
    result <- list()
    if (pillai) result$manova <- summary(manova(model))
    result$anovas <- MultipleANOVAs(data.frame(outcomes),
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        compare = "To mean",
                        correction = "None",
                        alernative = "Two-sided",
                        show.labels = show.labels,
                        p.cutoff = p.cutoff,
                        seed = seed,
                        ...)
    # Performing FDR correction.
    ps <- unlist(lapply(result$anovas, function(x) x$coefs[, 4]))
    if (fdr)
    {
        ps <- p.adjust(ps, method = "fdr")
        n.pars <- length(ps) / n.variables
        for (a in 1:n.variables)
        {
            var.ps <- ps[(a - 1)*n.pars + 1:n.pars]
            result$anovas[[a]]$coefs[, 4] <- var.ps
            result$anovas[[a]]$p <- min(var.ps)
        }
    }
    # Tidying up outputs
    if (show.labels)
        names(result$anovas) <- labels
    class(result) <- "OneWayMANOVA"
    result$title <- paste0("MANOVA: ", predictor.label)

    result$p <- p <- if (pillai) result$manova$stats[1,6] else min(ps)
    if (pillai)
        result$pillai <- result$manova$stats[1,2]
    subtitle <- if (p <= p.cutoff) "Significant" else "Not significant"
    result$subtitle <- paste0(subtitle, " - ", if (pillai)
                    paste0("Pillai's Trace: ", FormatAsReal(result$pillai, 3), ", approximate p-value: ")
                else
                    paste0("Smallest p-value", (if (fdr) " (after applying False Discovery Rate correction)"),  ": "),
                              FormatAsPValue(p))
    result$footer <- footer
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
        footer = x$footer))
}

