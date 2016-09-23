#' \code{OneWayANOVA}
#'
#' Computes a one-way analysis of variance with post hoc tests.
#' @param outcome The outcome variable.
#' @param groups The factor representing the groups.
#' @param rows The list of factors representing the rows.
#' @param compare One of \code{"To mean", "Pairwise", "To first"} (which implement's Dunnett's C, when
#' combined with 'correction' == 'Tukey Range'), or \code{"All"}
#' @param correction The multiple comparison adjustment method: \code{"Tukey Range", "None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"} (Westfall et al. 1999), \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} (Bretz et al. 2010) \code{"Shaffer"}, and \code{"Westfall"}.
#' @param robust.se Computes heteroscedastic-consistent standard errors, using the
#' \code{sandwich} package (Zeileis, 2004, 2006), that are robust to violations of
#'   the assumption of constant variance.
#' @param alternative The alternative hypothesis: "Two sided", "Greater", or "Less". The main application of this is when
#' Compare us set 'To first' (e.g., if testing a new product, where the purpose is to work out of the new product is superior
#' to an existing product, "Greater" would be chosen).
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
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param ... Other parameters to be passed to wrapped functions.
#' @details When 'Tukey Range' is selected, p-values are computed using t'tests, with a correction for the family-wise
#' error rate such that the p-values are correct for the largest range of values being compared (i.e.,
#' the biggest difference between the smallest and largest means). This is a single-step test. The method
#' of calculation is valid for both balanced and unbalanced samples (Bretz et al. 2011), and consequently the results
#' may differ for unbalanced samples to those that appear in most software and books (which instead employee an approximation
#' when the samples are unbalanced).
#' @references
#' Bretz,Frank, Torsten Hothorn and Peter Westfall (2011), Multiple Comparisons Using R, CRC Press, Boca Raton.
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
#' Tukey, John (1949). "Comparing Individual Means in the Analysis of Variance". Biometrics. 5 (2): 99–114. JSTOR 3001913.
#' Peter H. Westfall (1997), Multiple testing of general contrasts using logical constraints and correlations. Journal of the American Statistical Association, 92, 299–306.
#' P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). Multiple Comparisons and Multiple Tests Using the SAS System. Cary, NC: SAS Institute Inc.
#' Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics 48, 1005–1013.
#' Zeileis, Achim  (2004). Econometric computing with HC and HAC covariance matrix estimators.
#' Journal of Statistical Software, 11(10):1–17, URL http://www.jstatsoft.org/v11/i10/.
#' Zeileis, Achim  (2006). Achim Zeileis. Object-oriented computation of sandwich estimators. Journal of Statistical
#' Software, 16(9):1–16 URL http://www.jstatsoft.org/v16/i09/
    #' @importFrom flipRegression Regression GrandMean
#' @importFrom flipTransformations AsNumeric Factor
#' @importFrom multcomp glht mcp adjusted
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable
#' @importFrom survey regTermTest
#' @importFrom car hccm
#' @export
OneWayANOVA <- function(outcome,
                        groups,
                        compare = "Pairwise",
                        correction = "Tukey Range",
                        robust.se = FALSE,
                        alternative = "Two-sided",
                        show.labels = TRUE,
                        output = "Pretty",
                        outcome.name = NULL,
                        p.cutoff = 0.05,
                        seed = 1223,
                        weights = NULL,
                        ...)
{
    correct <- correction
    no.correction <- correction == "None"
    alt <- alternative
    alternative <- switch(alternative, "Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")
    correction <- switch(correction, "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", "Bonferroni" = "bonferroni",
        "Benjamini & Yekutieli" = "BY","False Discovery Rate" = "fdr", "None" = "none", "Single-step" = "single-step",
        "Shaffer" = "Shaffer", "Westfall" = "Westfall", "Free Combinations" = "free",
        "Tukey Range" = "none", "Dunnett" = "none")

    group.label <- Labels(groups)
    groups <- tidyFactor(groups)
    outcome.name = if(is.null(outcome.name)) deparse(substitute(outcome)) else outcome.name
    outcome.label = if (show.labels & !is.null(attr(outcome, "label"))) attr(outcome, "label") else outcome.name
    n.groups <- nlevels(groups)
    # Should be removed when hooking up GLM
    outcome <- if (is.factor(outcome)) AsNumeric(outcome, binary = FALSE) else outcome
    regression <- Regression(outcome ~ groups,
                             weights = weights,
                             subset = list(...)$subset,
                             type = if (is.null(list(...)$type)) "Linear" else list(...)$type,
                             robust.se = robust.se,
                             missing = if (is.null(list(...)$missing)) "Exclude cases with missing data" else list(...)$missing)
    model <- regression$original
    robust.se <- if (regression$robust.se) "; robust standard errors" else "" # Taking from regression, as regression checks for weight.
    contrasts <- mcp(groups = switch(compare, "Pairwise" = "Tukey", "To mean" = "GrandMean", "To first" = "Dunnett"))
    comparisons <- glht(model, linfct = contrasts, alternative = alternative)
    # comparisons <- if (robust.se)
    #                     glht(model, linfct = contrasts, vcov = hccm(model, type = "hc1"), alternative = alternative)
    #                 else
    #                     glht(model, linfct = contrasts, alternative = alternative)
    set.seed(seed)
    mcomp <- if (correct == "Tukey Range")
                    summary(comparisons)
                else
                    suppressWarnings(summary(comparisons, test = adjusted(type = correction)))
    result <- list(original = mcomp)
    result$grand.mean <- GrandMean(regression)
    result$correction <- correct
    result$n <- table(groups[regression$subset])
    result$outcome.label <- outcome.label
    result$r.squared <- regression$r.squared
    # Extracting/Computing F-test of equal parameters.
    weighted <- inherits(model, "svyglm")
    result$f.test <- f.test <- if(weighted) politeWeightedFTest(model) else summary(aov(model))
    result$f <- f <- if(weighted) f.test$Ftest else regression$summary$fstatistic[1]
    result$df <- df <- if(weighted) f.test$df else regression$summary$fstatistic[2]
    result$ddf <- ddf <- if(weighted) f.test$ddf else regression$summary$fstatistic[3]
    result$p <- p <- if(weighted) f.test$p else 1 - pf(f, df, ddf)
    result$subtitle <- paste0("F = ", FormatAsReal(f, 4),
             " on ", df, " and ", ddf, " degrees-of-freedom: p = ", FormatAsPValue(p),
             "; R-squared: ", FormatAsReal(regression$r.squared, 4))
    result$title <-paste0("One-way ANOVA: ", outcome.label, " by ", group.label)
    mc.correction <- paste0("; multiple comparisons correction: ", correct)
    alpha <- paste0("results highlighted when ", (if (no.correction) "" else "corrected "),"p <= " , p.cutoff, "; null hypothesis: ", tolower(alt))
    result$footer <- paste0(regression$sample.description,
                            alpha,mc.correction,robust.se)
    result$outcome.label <- outcome.label
    result$column.names <- levels(groups)
    result$compare <- compare
    result$output <- output
    result$r.squared <- regression$r.squared

    r <- result$original$test
    if (no.correction & compare == "To first")
    {# Using more precise results from Regression rather than glht
        rcoefs <- regression$summary$coefficients[-1, , drop = FALSE]
        k <- nrow(rcoefs)
        r$coefficients[1:k] <- rcoefs[1:k, 1] # Retaining the labels.
        r$sigma <- rcoefs[, 2]
        if (sum(abs(r$tstat - rcoefs[, 3]) > .1))
        {
            print(rbind(svyglm = r$tstat, multcomp = rcoefs[, 3]))
            stop("Unreliable inference in one-way ANOVA.")
        }
        r$tstat <- rcoefs[, 3]
        r$pvalues <- rcoefs[, 4]
    }
    coefs <- r$coefficients + if (compare == "To mean") result$grand.mean else 0
    coefs <- cbind(coefs, r$sigma, r$tstat, r$pvalues)
    colnames(coefs) <- colnames(regression$summary$coefficients)
    if (compare == "To mean")
    {
        rownames(coefs) <- levels(groups)
        colnames(coefs)[1] <- "Mean"
    }
    else
        colnames(coefs)[1] <- "Difference"
    if (!no.correction)
        colnames(coefs)[4] <- "Corrected p"
    result$coefs <- coefs
    class(result) <- "OneWayANOVA"
    result
}


politeWeightedFTest <- function(model)
{
    .errorInTest <- function(test)
    {
        if (any("try-error" %in% class(test)))
            return(TRUE)
        FALSE
    }
    test <- suppressWarnings(try(regTermTest(model, "groups")))
    if(.errorInTest(test))
    {
        warning("Unable to compute weighted F-test; probably due to a problem with the data (e.g., too small sample size in a group).")
        return(list(Ftest = NA, df = NA, ddf = NA, p = NA))
    }
    test
}

#' @export
print.OneWayANOVA <- function(x, ...)
{
    is.t <- x$original$df > 0
    estimate.name <- if (x$compare == "To mean") "Estimate" else "Difference"
    p.name <- if(x$correction == "NONE")
        "<span style='font-style:italic;'>p</span>"
    else
        "Corrected <span style='font-style:italic;'>p</span>"

    dt <- RegressionTable(x$coefs,
                          title = x$title,
                          subtitle = x$subtitle,
                          footer = x$footer,
                          estimate.name = estimate.name,
                          p.name = p.name)
    print(dt)
    invisible(x)
}
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
