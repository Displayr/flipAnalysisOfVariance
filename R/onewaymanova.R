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
#'   \code{"Error if missing data"}, \code{"Use partial data"}, and
#'   \code{"Exclude cases with missing data"}. Option \code{"Use partial data"} will
#' run separate one way ANOVAs with missing data filtered out for each outcome variable
#' separately.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param p.cutoff The alpha level to be used in testing.
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param binary Automatically converts non-ordered factors to dummy-coded (binary indicator) variables.
#' @param pillai If \code{TRUE}, Pillai's Trace is computed as the overall MANOVA statistic.
#' @param fdr If \code{TRUE}, the False Discovery Rate correction is applied. This is the default.
#' @param return.all If \code{TRUE}, returns all the internal computations in the output object. If \code{FALSE},
#' returns just the information required to print the output.
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
#' @importFrom flipTransformations AsNumeric Factor ProcessQVariables
#' @importFrom flipData CheckForLinearDependence CalibrateWeight
#' @importFrom multcomp glht mcp adjusted
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable OriginalName ExtractCommonPrefix SampleDescription
#' @importFrom survey regTermTest
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
                        return.all = FALSE,
                        ...)
{
    predictor.label <- if(show.labels) Labels(predictor) else OriginalName(predictor)

    outcomes <- ProcessQVariables(outcomes)
    predictor <- ProcessQVariables(predictor)
    weighted <- !is.null(weights)

    # Removing missing values and filtering weights.
    dat <- prepareData(outcomes, predictor, NULL, subset, weights, binary, missing)
    result <- list()
    args <- list(subset = NULL, weights = weights, compare = "To mean",
                 correction = if(pillai) "Tukey Range" else "Table FDR",
                 alernative = "Two-sided", show.labels = show.labels,
                 p.cutoff = p.cutoff, seed = seed, return.all = return.all,
                 robust.se = robust.se)

    ## Setup data for calling MultipleANOVAS
    if (!is.data.frame(dat))
    {
        df <- do.call(rbind, dat)
        df <- as.data.frame(df)
        if (is.null(colnames(outcomes)))
            colnames(outcomes) <- paste0("X", ncol(outcomes))
        lens <- vapply(dat, nrow, 0L)
        df <- cbind(df, unlist(mapply(rep, colnames(outcomes), lens,
                                      SIMPLIFY = FALSE, USE.NAMES = FALSE)))
        df <- df[, -3]  # drop useless covariate column
        if (!weighted)
            df <- df[, - 3]
        names(df) <- c("dependent", "independent", if (weighted) "weights",
                       "dependent.name")
        args$data <- df
        n.outcomes <- ncol(outcomes)
  }else
  {
      n.outcomes <- length(dat) - 3
      predictor <- dat[[n.outcomes + 1]]
      outcomes <- dat[, 1:n.outcomes, drop = FALSE]
      weights <- if (weighted)
                     dat[[n.outcomes + 3]]
                 else NULL
      args$dependents  <- data.frame(outcomes)
      args$independent <- predictor
      args$weights <- weights
    }
    result$anovas <- do.call(MultipleANOVAs, args)

    footer <- attr(dat, "footer")

    labels <- attr(dat, "labels")[1:ncol(outcomes)]
    ## footer <- paste0(footer, "two-sided comparisons against the row means",
    ## if (fdr) " (p-values corrected using False Discovery Rate)")

    if (pillai)
    {
        if (!is.data.frame(dat))  # partial data, can't compute Pillai Trace
            stop("Pillai's Trace cannot be computed with partial data. ",
                 "Please set 'pillai' to 'FALSE' to run the analysis.")
        result$manova <- computePillai(outcomes, predictor,  weighted, dat, n.outcomes,
                                       weights)
    }

    ps <- attr(result$anovas, "ps")
    # Tidying up outputs
    if (show.labels)
        names(result$anovas) <- labels

                                        # Extract common prefix from outcomes
    extracted <- ExtractCommonPrefix(names(result$anovas))
    if (!is.na(extracted$common.prefix))
    {
        result$title <- paste0("MANOVA: ", extracted$common.prefix, " by ", predictor.label)
        names(result$anovas) <- extracted$shortened.labels
    }
    else
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
    result$table <- FormattableANOVAs(result$anovas,
                    title = result$title,
                    subtitle = result$subtitle,
                    footer = result$footer)
    if (!return.all)
        result <- list(table = result$table)
    class(result) <- "OneWayMANOVA"
    result
}

computePillai <- function(outcomes, predictor, weighted, df, n.variables, weights)
{
    if (weighted)
    {
        wgt <- CalibrateWeight(df[, n.variables + 3])
        df <- AdjustDataToReflectWeights(df[, 1:(n.variables + 1)], weights)
        o.matrix <- as.matrix(df[, 1:n.variables])
        CheckForLinearDependence(o.matrix)
        g <- df[, n.variables + 1]
        g <- removeMissingLevels(g)
        warning("Pillai's trace cannot be correctly computed with weighted data.")
        model <- lm(o.matrix ~ g)
    }
    else
    {
        o.matrix <- as.matrix(outcomes)
        CheckForLinearDependence(o.matrix)
        predictor <- removeMissingLevels(predictor)
        model <- lm(o.matrix ~ predictor)
    }
    return(summary(manova(model)))
}

#' @importFrom flipTransformations Factor
#' @return A data.frame containing the outcomes
#' @noRd
prepareData <- function(outcomes, predictor, covariate, subset, weights, binary, missing)
{
    n.total <- length(predictor)
    use.partial <- missing == "Use partial data"
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
    if (is.null(covariate))
        covariate <- rep(-1, n.total) # A fudge to ensure complete.cases does not fail

    makeDF <- function(outcomes, predictor, covariate, weights)
    {
        out <- cbind(as.data.frame(outcomes), Factor(predictor), Factor(covariate), weights)
        out <- out[subset & complete.cases(out) & weights > 0, ]
        out
    }

    any.na <- anyNA(predictor) || anyNA(subset) || anyNA(outcomes) || anyNA(weights)
    if (!use.partial || !any.na)
    {
        if (missing == "Error if missing data" && any.na)
            stop("By default, MANOVA only operates on complete data, but the supplied data ",
             "contains missing values. Change 'Missing data' to 'Exclude cases with missing data' ",
             "or 'Use partial data' to run the analysis.")

        out <- makeDF(outcomes, predictor, covariate, weights)
        n.estimation <- nrow(out)
        if (n.estimation == 0)
            stop("After removing cases with missing data, there are no ",
                 "observations to use in the ANOVA. Switch 'Missing Data' to ",
                 "'Use partial data' to run the analysis.")
    }else
    {  # use partial data
        out <- lapply(outcomes, makeDF, predictor, covariate, weights)
        n.estimation <- min(vapply(out, nrow, 0L))
        if (n.estimation == 0)
            stop("After removing observations with missing data, there are no ",
                 "observations to use in the ANOVA for at least one outcome variable.")
        if (length(out) == 1L)
            out <- out[[1L]]
    }

    attr(out, "footer") <- SampleDescription(n.total, n.subset, n.estimation,
                                             subset.label, weighted, weight.label, missing = "",
                                             imputation.label = NULL, NULL)
    attr(out, "labels") <- Labels(outcomes)
    out
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
    print(x$table)
}

