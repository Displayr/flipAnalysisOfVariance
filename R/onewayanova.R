#' \code{OneWayANOVA}
#'
#' Computes a one-way analysis of variance with post hoc tests.
#' @param outcome The outcome variable.
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
#' "Free Combinations"} (Westfall et al. 1999), \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} (Bretz et al. 2010) \code{"Shaffer"}, and \code{"Westfall"}.
#' @param alternative The alternative hypothesis: "Two sided", "Greater", or "Less". The main application of this is when
#' Compare us set 'To first' (e.g., if testing a new product, where the purpose is to work out of the new product is superior
#' to an existing product, "Greater" would be chosen).
#' @param robust.se If \code{TRUE}, computes standard errors that are robust to violations of
#'   the assumption of constant variance for linear and Poisson models, using the HC3 modification of White's (1980) estimator
#'   (Long and Ervin, 2000). This parameter is ignored if weights are applied (as weights already
#'   employ a sandwich estimator). Other options are \code{FALSE} and \code{"FALSE"No}, which do the same
#'   thing, and \code{"hc0"}, \code{"hc1"}, \code{"hc2"}, \code{"hc4"}.
#' @param missing How missing data is to be treated in the ANOVA. Options:
#'   \code{"Error if missing data"}.
#'   \code{"Exclude cases with missing data"}, and
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param outcome.name The name of the outcome  variable. Only used when \code{show.labels} is FALSE. Defaults to
#' the actual variable name, which does not work so well if the function is being called by another function.
#' @param predictor.name The name of the predictor variable. Only used when \code{show.labels} is FALSE. Defaults to
#' the actual variable name, which does not work so well if the function is being called by another function.
#' @param p.cutoff The alpha level to be used in testing.
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param return.all If \code{TRUE}, returns all the internal computations in the output object. If \code{FALSE},
#' returns just the information required to print the output.
#' @param ... Other parameters to be passed to wrapped functions.
#' @details When 'Tukey Range' is selected, p-values are computed using t'tests, with a correction for the family-wise
#' error rate such that the p-values are correct for the largest range of values being compared (i.e.,
#' the biggest difference between the smallest and largest means). This is a single-step test. The method
#' of calculation is valid for both balanced and unbalanced samples (Bretz et al. 2011), and consequently the results
#' may differ for unbalanced samples to those that appear in most software and books (which instead employee an approximation
#' when the samples are unbalanced).
#'
#' When \code{missing = "Imputation (replace missing values with estimates)"}, all selected
#' outcome and predictor variables are included in the imputation, along with
#' all \code{auxiliary.data}, excluding cases that are excluded via subset or
#' have invalid weights, but including cases with missing values of the outcome variable.
#' Then, cases with missing values in the outcome variable are excluded from
#' the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#'
#' @references
#' Bretz,Frank, Torsten Hothorn and Peter Westfall (2011), Multiple Comparisons Using R, CRC Press, Boca Raton.
#' Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B 57, 289-300.
#' Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29, 1165-1188.
#' Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics 6, 65-70.
#' Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika 75, 800-803.
#' Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika 75, 383-386.
#' Hothorn, Torsten, Frank Bretz and Peter Westfall (2008), Simultaneous Inference in General Parametric Models. Biometrical Journal, 50(3), 346-363.
#' Long, J. S. and Ervin, L. H. (2000). Using
#' heteroscedasticity consistent standard errors in the linear regression
#' model. The American Statistician, 54(3): 217-224.
#' Shaffer, Juliet P. (1986), Modified sequentially rejective multiple test procedures. Journal of the American Statistical Association, 81, 826-831.
#' Shaffer, Juliet P. (1995). Multiple hypothesis testing. Annual Review of Psychology 46, 561-576.
#' Sarkar, S. (1998). Some probability inequalities for ordered MTP2 random variables: a proof of Simes conjecture. Annals of Statistics 26, 494-504.
#' Sarkar, S., and Chang, C. K. (1997). Simes' method for multiple hypothesis testing with positively dependent test statistics. Journal of the American Statistical Association 92, 1601-1608.
#' Tukey, John (1949). "Comparing Individual Means in the Analysis of Variance". Biometrics. 5 (2): 99-114.
#' Peter H. Westfall (1997), Multiple testing of general contrasts using logical constraints and correlations. Journal of the American Statistical Association, 92, 299-306.
#' P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). Multiple Comparisons and Multiple Tests Using the SAS System. Cary, NC: SAS Institute Inc.
#' von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#' Methodology 37:83-117.
#' Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics 48, 1005-1013.
#' White, H. (1980), A heteroskedastic-consistent covariance matrix estimator and a direct test of heteroskedasticity.
#' Econometrica, 48, 817-838.
#' @importFrom flipData Observed
#' @importFrom flipFormat Labels FormatAsReal FormatAsPValue RegressionTable OriginalName
#' @import flipRegression
# #Regression GrandMean vcov.Regression
#' @importFrom flipStatistics Frequency
#' @importFrom flipTransformations AsNumeric Factor ProcessQVariables
#' @importFrom flipRegression PValueAdjustFDR
#' @importFrom multcomp glht mcp adjusted
#' @importFrom survey regTermTest
#' @importFrom stats aov pf vcov ptukey
#' @importFrom verbs Sum
#' @export
OneWayANOVA <- function(outcome,
                        predictor,
                        subset = NULL,
                        weights = NULL,
                        compare = "Pairwise",
                        correction = "Tukey Range",
                        alternative = "Two-sided",
                        robust.se = FALSE,
                        missing = "Exclude cases with missing data",
                        show.labels = TRUE,
                        outcome.name = NULL,
                        predictor.name = NULL,
                        p.cutoff = 0.05,
                        seed = 1223,
                        return.all = FALSE,
                        ...)
{
    outcome <- ProcessQVariables(outcome)
    predictor <- ProcessQVariables(predictor)
    ## DS-2353 Ensure vector of unity weights treated same as NULL weights
    if (!is.null(weights) && all(weights == 1))
        weights <- NULL

    if (robust.se == "No")
        robust.se <- FALSE
    .multcompSummary <- function(comparisons, correct, robust.se)
    {
        if (correct == "Tukey Range")
        {
            if (compare == "Pairwise" && comparisons$alternative == "two.sided" && !robust.se)
            {
                res <- summary(comparisons, test = adjusted(type = "none"))

                # Overwrite pvalues with adjusted
                compare <- attr(comparisons$linfct, "type")
                MSE <- Sum(comparisons$model$residuals^2, remove.missing = FALSE) / comparisons$model$df.residual
                means <- comparisons$model$coefficients
                means[-1] <- means[-1] + means[1]
                counts <- table(comparisons$model$model$predictor)

                center <- outer(means, means, "-")
                keep <- lower.tri(center)
                center <- center[keep]
                est <- center / (sqrt((MSE/2) * outer(1/counts, 1/counts, "+"))[keep])
                pvals <- ptukey(abs(est), length(means), comparisons$model$df.residual, lower.tail = FALSE)
                res$test$pvalues <- pvals
                return(res)
            }
            res <- tryCatch({summary(comparisons)},
                            warning = function(w) {warning("Numerical precision of p-value calcuations exceeds threshold. ",
                                                           "Treat p-values with caution.")
                                suppressWarnings(summary(comparisons))})
            return(res)
        }
        if (correct == "False Discovery Rate" || correct == "fdr")
        {
            res <- summary(comparisons, test = adjusted(type = "none"))
            res$test$pvalues <- PValueAdjustFDR(res$test$pvalues)
            return(res)
        }
        return(summary(comparisons, test = adjusted(type = correction)))
    }
    if (is.null(outcome.name))
        outcome.name <- OriginalName(outcome)
    if (is.null(predictor.name))
        predictor.name <- OriginalName(predictor)
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    correct <- correction
    no.correction <- correction == "None"
    alt <- alternative
    alternative <- switch(alternative, "Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")
    correction <- switch(correction, "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", "Bonferroni" = "bonferroni",
                         "Benjamini & Yekutieli" = "BY","False Discovery Rate" = "fdr", "None" = "none", "Single-step" = "single-step",
                         "Shaffer" = "Shaffer", "Westfall" = "Westfall", "Free Combinations" = "free",
                         "Tukey Range" = "none", "Dunnett" = "none")
    predictor.label <- if (show.labels) Labels(predictor) else predictor.name
    predictor <- tidyFactor(predictor)
    #outcome.label = if (show.labels & !is.null(attr(outcome, "label"))) attr(outcome, "label") else outcome.name
    outcome.label = if (show.labels) Labels(outcome) else outcome.name
    # Should be removed when hooking up GLM
    outcome <- if (is.factor(outcome)) AsNumeric(outcome, binary = FALSE) else outcome
    regression <- Regression(outcome ~ predictor,
                             subset = subset,
                             missing = missing,
                             weights = weights,
                             type = if (is.null(list(...)$type)) "Linear" else list(...)$type,
                             robust.se = robust.se,
                             internal = TRUE)
    model <- regression$original
    robust.se <- regression$robust.se
    contrasts <- mcp(predictor = switch(compare, "Pairwise" = "Tukey", "To mean" = "GrandMean", "To first" = "Dunnett"))
    vcov <- try(vcov(regression, robust.se), silent = TRUE)
    if (!tryError(vcov))
    {
        comparisons <- glht(model, linfct = contrasts, alternative = alternative, vcov = vcov)
        set.seed(seed)
        mcomp <- try(.multcompSummary(comparisons, correct, robust.se), silent = TRUE)
    }
    if (tryError(vcov) || tryError(mcomp))
    {
        if(robust.se)
        {
            warning("Robust standard errors have not been implemented for this model.")
            mcomp <- .multcompSummary(glht(model, linfct = contrasts, alternative = alternative), correct, FALSE)
        }
        else if (!is.null(weights))
            stop("Weights cannot be used with this model. Remove weights to compute the unweighted model.")
    }
    f.test <- FTest(regression)
    sub <- if (is.null(subset)) rep(TRUE, length(predictor)) else subset
    if (!is.null(weights))
        sub <- sub & weights > 0
    predictor.n <- Frequency(predictor, sub)
    predictor.n <- predictor.n[predictor.n > 0]
    result <- c(f.test,
                list(original = mcomp,
                     robust.se = robust.se,
                     grand.mean = GrandMean(regression),
                     correction = correct,
                     outcome.label = outcome.label,
                     r.squared = rSquared(regression),
                     p.cutoff = p.cutoff,
                     compare = compare,
                     column.names = names(predictor.n),
                     n = predictor.n))
    # Headers, subtitles, footers
    mc.correction <- paste0("; multiple comparisons correction: ", correct)
    alpha <- paste0("null hypotheses: ", tolower(alt))
    robust.se.text <- if (robust.se == FALSE) "" else
        paste0("; heteroscedasticity-robust standard errors (", if(robust.se == TRUE) "hc3" else robust.se, ");")
    result$posthoc <- paste0(alpha, mc.correction, robust.se.text)
    result$subtitle <- if (is.na(f.test$p)) "Error computing p-value" else paste0(if (f.test$p <= p.cutoff) "Significant" else "Not significant",
                                                                                  ": F: ", FormatAsReal(f.test$Ftest, 4),
                                                                                  " on ", f.test$df, " and ", f.test$ddf, " degrees-of-freedom; p: ", FormatAsPValue(f.test$p),
                                                                                  "; R-squared: ", FormatAsReal(result$r.squared, 4))
    result$title <- paste0("One-way ANOVA: ", outcome.label, " by ", predictor.label)
    result$footer <- paste0(regression$sample.description, result$posthoc)
    r <- result$original$test
    if (no.correction & compare == "To first")
    {   # Using more precise results from Regression rather than glht
        rcoefs <- summary(model)$coef[-1, , drop = FALSE]
        k <- nrow(rcoefs)
        r$coefficients[1:k] <- rcoefs[1:k, 1] # Retaining the labels.
        r$sigma <- rcoefs[, 2]
        if (Sum(abs(r$tstat - rcoefs[, 3]) > .1, remove.missing = FALSE))
        {
            print(rbind(svyglm = r$tstat, multcomp = rcoefs[, 3]))
            stop("Unreliable inference in one-way ANOVA.")
        }
        r$tstat <- rcoefs[, 3]
        r$pvalues <- rcoefs[, 4]
    }
    coefs <- r$coefficients + if (compare == "To mean") result$grand.mean else 0
    #coef.names <- gsub("predictor", "", names(model$coef)[-1])
    coefs <- cbind(coefs, r$sigma, r$tstat, r$pvalues)
    colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|))")
    if (compare == "To mean")
    {
        tbl <- table(predictor[regression$subset])
        nms <- names(tbl)[tbl > 0]
        rownames(coefs) <- nms
        colnames(coefs)[1] <- "Mean"
    }
    else
        colnames(coefs)[1] <- "Difference"
    if (!no.correction)
        colnames(coefs)[4] <- "Corrected p"
    result$coefs <- coefs
    # Creating the final outputs.
    is.t <- result$original$df > 0
    estimate.name <- if (result$compare == "To mean") "Estimate" else "Difference"
    p.name <- if(result$correction == "NONE")
        "<span style='font-style:italic;'>p</span>"
    else
        "Corrected <span style='font-style:italic;'>p</span>"
    result$table <- RegressionTable(result$coefs,
                                    title = result$title,
                                    subtitle = result$subtitle,
                                    footer = result$footer,
                                    estimate.name = estimate.name,
                                    se.name = if (result$robust.se) "Robust SE" else "Standard Error",
                                    p.name = p.name,
                                    p.cutoff = result$p.cutoff)
    coefs <- result$coefs
    if (!return.all)
        result <- list(table = result$table)
    attr(result, "ChartData") <- coefs
    class(result) <- "OneWayANOVA"
    result
}

#' rSquared
#'
#' @param  object A \link{FitRegression} object.
#' @import flipRegression
#' @importFrom flipData Observed
#' @importFrom flipStatistics Correlation
#' @importFrom stats predict
rSquared <- function(object)
{
    predicted <- predict(object$original)
    observed <- Observed(object)
    cor <- Correlation(predicted, observed, object$weights)
    cor * cor
}


tryError <- function(x)
    inherits(x, "try-error")

#' politeWeightedFTest
#'
#' Computes \link{regTermTest}, but fails savely when an error occurs.
#' @param fit A FitRegression object.
#' @importFrom flipU OutcomeName
#' @importFrom survey regTermTest
politeWeightedFTest <- function(fit)
{
    test <- suppressWarnings(try(regTermTest(fit$original, "predictor"), silent = TRUE))
    if(tryError(test))
    {
        warning("Unable to compute weighted F-test due to a technical problem with the underlying computations. This error may disappear if you remove the weight.")
        return(list(Ftest = NA, df = NA, ddf = NA, p = NA))
    }
    test
}

#' \code{FTest}
#'
#' An F-Test from a \code{lm} or linear \code{svyglm} object.
#' @param fit An object created by \link{FitRegression}.
#' @importFrom stats aov
#' @export
FTest <- function(fit)
{
    if (!is.null(fit$weights))
        return(politeWeightedFTest(fit))
    aovFTest(fit$original)
}

aovFTest <- function(model)
{
    f.test <- summary(aov(model))[[1]]
    list(Ftest = f.test[1, 4],
         df = f.test[1, 1],
         ddf = f.test[2, 1],
         p = f.test[1, 5])
}

#' @export
print.OneWayANOVA <- function(x, ...)
{
    print(x$table)
}

