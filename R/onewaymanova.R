#' \code{OneWayMANOVA}
#'
#' Computes a one-way analysis of variance with post hoc tests.
#' @param outcomes The outcome variables, as a data frame.
#' @param groups The factor representing the groups.
#' @param rows The list of factors representing the rows.
#' @param correction The multiple comparison adjustment method: \code{"None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"} (Westfall et al. 1999), \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} (Bretz et al. 2010) \code{"Shaffer"}, and \code{"Westfall"}.
#' @param output One of \code{"Standard"}, \code{"ANOVA"}, or \code{"Table"}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"}, and
#'   \code{"Ordered Logit"}.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param p.cutoff The alpha level to be used in testing.
#' @param ... Other parameters to be passed to wrapped functions.
#' @references
#' Bretz,Frank, Torsten Hothorn and Peter Westfall (2010), Multiple Comparisons Using R, CRC Press, Boca Raton.
#' Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B 57, 289–300.
#' Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29, 1165–1188.
#' Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics 6, 65–70.
#' Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika 75, 800–803.
#' Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika 75, 383–386.
#' Hothorn, Torsten, Frank Bretz and Peter Westfall (2008), Simultaneous Inference in General Parametric Models. Biometrical Journal, 50(3), 346–363.
#' Shaffer, Juliet P. (1986), Modified sequentially rejective multiple test procedures. Journal of the American Statistical Association, 81, 826–831.
#' Shaffer, Juliet P. (1995). Multiple hypothesis testing. Annual Review of Psychology 46, 561–576.
#' Sarkar, S. (1998). Some probability inequalities for ordered MTP2 random variables: a proof of Simes conjecture. Annals of Statistics 26, 494–504.
#' Sarkar, S., and Chang, C. K. (1997). Simes' method for multiple hypothesis testing with positively dependent test statistics. Journal of the American Statistical Association 92, 1601–1608.
#' Peter H. Westfall (1997), Multiple testing of general contrasts using logical constraints and correlations. Journal of the American Statistical Association, 92, 299–306.
#' P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). Multiple Comparisons and Multiple Tests Using the SAS System. Cary, NC: SAS Institute Inc.
#' Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics 48, 1005–1013.
#' @importFrom flipRegression Regression GrandMean
#' @importFrom flipTransformations AsNumeric Factor
#' @importFrom multcomp glht mcp adjusted
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable
#' @importFrom survey regTermTest
#' @importFrom flipData SampleDescription
#' @export
OneWayMANOVA <- function(outcomes,
                        groups,
                        subset = NULL,
                        weights = NULL,
                        compare = "Pairwise",
                        correction = "False Discovery Rate",
                        show.labels = TRUE,
                        output = "Pretty",
                        outcome.name = NULL,
                        p.cutoff = 0.05,
                        binary = FALSE,
                        ...)
{
    # Removing missing values and filtering weights.
    n.total <- length(groups)
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
    group.label <- Labels(groups)
    df <- cbind(outcomes, groups, weights)
    df <- subset(df, subset = subset & complete.cases(df))
    n.estimation <- nrow(df)
    n.variables <- ncol(outcomes)
    groups <- df[, n.variables + 1]
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
        model <- lm(as.matrix(outcomes) ~ groups)
    result <- list(manova = summary(manova(model)))
    result$anovas <- MultipleANOVAs(outcomes = outcomes,
                        groups = groups,
                        subset = NULL,
                        weights = NULL,
                        compare = compare,
                        correction = correct,
                        show.labels = show.labels,
                        output = output,
                        outcome.name = outcome.name,
                        p.cutoff = p.cutoff,
                        ...)
    result$group.label <- group.label
    class(result) <- "OneWayMANOVA"
    result
    }


#' @importFrom flipFormat FormatAsReal FormatAsPValue
#' @export
print.OneWayMANOVA <- function(x, ...)
{
    printFormattableANOVAs(x$anovas,
        title = paste0("MANOVA: ",x$group.label),
        subtitle = paste0("Pillai's Trace: ", FormatAsReal(x$manova$stats[1,2], 3), ", approximate p-value: ", FormatAsPValue(x$manova$stats[1,6])),
        footer = paste0(x$anovas[[1]]$footer, " (corrections performed within rows)"))

}

#' @importFrom flipFormat MeanComparisonsTable
#' @export
printFormattableANOVAs <- function(anovas, title, subtitle, footer)
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


#' #' @export
#' print.OneWayANOVA <- function(x, ...)
#' {
#'     is.t <- x$original$df > 0
#'     estimate.name <- if (x$compare == "Cells") "Estimate" else "Difference"
#'     dt <- RegressionTable(x$coefs,
#'                           title = x$title,
#'                           subtitle = x$subtitle,
#'                           footer = x$footer,
#'                           estimate.name = estimate.name,)
#'     print(dt)
#'     invisible(x)
#' }
#'
#'
#'         mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
#'         error <- attr(pq$pvalues, "error")
#'         pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==
#'             0, "z", "t"), ")", sep = ""), greater = paste("Pr(>",
#'             ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",
#'             ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
#'         colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==
#'             0, "z value", "t value"), pname)
#'         type <- pq$type
#'         if (!is.null(error) && error > .Machine$double.eps) {
#'             sig <- which.min(abs(1/error - (10^(1:10))))
#'             sig <- 1/(10^sig)
#'
#'     #' @importFrom flipFormat MeanComparisonsTable
#'     #' @export
#'     print.CompareMultipleMeans <- function(x, ...)
#'     {
#'         mt <- MeansTables(x)
#'         mct <- MeanComparisonsTable(
#'             means = mt$means,
#'             zs = mt$zs,
#'             ps = mt$ps,
#'             r.squared = mt$r.squared,
#'             overall.p = mt$overall.p,
#'             column.names = mt$column.names,
#'             footer = ""
#'         )
#'         print(mct)
#'         invisible(x)
#'     }
#' }

#
# partial.coefs <- cbind(beta, se, original$t, original$Probability)
# dimnames(partial.coefs) <- list(variable.names[predictors.index],
#                                 c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))



#     if (!is.null(x$type))
#         cat("Multiple Comparisons of Means:", x$type, "Contrasts\n\n\n")
#     call <- if (isS4(x$model))
#         x$model@call
#     else x$model$call
#     if (!is.null(call)) {
#         cat("Fit: ")
#         print(call)
#         cat("\n")
#     }
#     pq <- x$test
#     mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
#     error <- attr(pq$pvalues, "error")
#     pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==
#         0, "z", "t"), ")", sep = ""), greater = paste("Pr(>",
#         ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",
#         ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
#     colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==
#         0, "z value", "t value"), pname)
#     type <- pq$type
#     if (!is.null(error) && error > .Machine$double.eps) {
#         sig <- which.min(abs(1/error - (10^(1:10))))
#         sig <- 1/(10^sig)
#     }
#     else {
#         sig <- .Machine$double.eps
#     }
#     cat("Linear Hypotheses:\n")
#     alt <- switch(x$alternative, two.sided = "==", less = ">=",
#         greater = "<=")
#     rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
#     printCoefmat(mtests, digits = digits, has.Pvalue = TRUE,
#         P.values = TRUE, eps.Pvalue = sig)
#     switch(type, univariate = cat("(Univariate p values reported)"),
#         `single-step` = cat("(Adjusted p values reported -- single-step method)"),
#         Shaffer = cat("(Adjusted p values reported -- Shaffer method)"),
#         Westfall = cat("(Adjusted p values reported -- Westfall method)"),
#         cat("(Adjusted p values reported --", type, "method)"))
#     cat("\n\n")
#     invisible(x)
