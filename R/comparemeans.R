#' \code{CompareMeans}
#' Performs statistical tests comparing means of an outcome variable.
#' @param outcome The outcome variable.
#' @param columns The factor representing the columns.
#' @param rows The list of factors representing the rows.
#' @param compare One of \code{"Rows", "Columns", "Columns pairwise"}, or \code{"All"}
#' @param correction The multiple comparison adjustment method: \code{"None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"} (Westfall et al. 1999), \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} (Bretz et al. 2010) \code{"Shaffer"}, and \code{"Westfall"}.
#' @param output One of \code{"Pretty"} or \code{"Table"}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"}, and
#'   \code{"Ordered Logit"}.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#'   \code{"Use partial data (pairwise correlations)"},
#'   \code{"Imputation (replace missing values with estimates)"}, and
#'   \code{"Multiple imputation"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
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
#' @importFrom flipFormat Labels
#' @importFrom survey regTermTest
#' @export
CompareMeans <- function(outcome,
                         columns,
                         Rows = NULL,
                         compare = "Columns",
                         correction = "False Discovery Rate",
                         show.labels = TRUE,
                         output = "Pretty",
                         outcome.name = NULL,
                         ...)
{
    if (length(Rows) == 1)
    {
        variable <- Factor(Rows[[1]])
        variable.name <- names(Rows)
        levs <- levels(variable)
        k <- nlevels(variable)
        outcomes <- vector("list")
        for (i in 1:k)
        {
            temp.outcome <- outcome
            print(class(outcome))
            lev <- levs[k]
            temp.outcome[variable != lev | is.na(variable)] <- NA
            nm <- paste0(Labels(outcome), ": ", lev)
            attr(temp.outcome, "label") <- nm
            print(table(temp.outcome))
            print(i)
            print(outcomes)
            outcomes[[i]] <- temp.outcome
            names(outcomes)[i] <- nm

        }
        return(CompareMultipleMeans(outcomes = outcomes,
                                     columns = columns,
                                     Rows = NULL,
                                     compare = compare,
                                     correction = correction,
                                     show.labels = show.labels,
                                    ...))
    }
    correction <- switch(correction, "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", "Bonferroni" = "bonferroni",
        "Benjamini & Yekutieli" = "BY","False Discovery Rate" = "fdr", "None" = "none", "Single-step" = "single-step",
        "Shaffer" = "Shaffer", "Westfall" = "Westfall", "Free Combinations" = "free")
    columns <- tidyFactor(columns)
    outcome.name = if(is.null(outcome.name)) deparse(substitute(outcome)) else outcome.name
    outcome.label = if (show.labels & !is.null(attr(outcome, "label"))) attr(outcome, "label") else outcome.name
    n.columns <- nlevels(columns)
    #column.names <- levels(columns)
    #levels(columns) <- 1:n.columns # Using integers to make it easier to match things.
    # Should be removed when hooking up GLM
    outcome <- if (is.factor(outcome)) AsNumeric(outcome, binary = FALSE) else outcome
    if (is.null(Rows))
    {
        if (any(compare %in% c("Rows", "All")))
            stop("To compare 'Rows' or 'All', you need to specify an input for 'Rows'.")

        t <- if (is.null(list(...)$type)) "Linear" else list(...)$type
        m <- if (is.null(list(...)$missing)) "Exclude cases with missing data" else list(...)$missing
        regression <- Regression(outcome ~ columns,
                                 weights = list(...)$weights,
                                 subset = list(...)$subset,
                                type = t,
                                 missing = m)
        # model$outcome.name <- outcome.name
        # model$outcome.name <- regression$outcome.variable
        # model$weights <- regression$outcome.variable
        model <- regression$original
        contrasts <- mcp(columns = switch(compare, "Columns pairwise" = "Tukey", "Columns" = "GrandMean"))
        comparisons <- glht(model, linfct = contrasts)
        result <- summary(comparisons, test = adjusted(type = correction))
        result$grand.mean <- GrandMean(regression)
        result$n <- table(columns[regression$subset])
        result$outcome.label <- outcome.label
        result$r.squared <- regression$r.squared
        if (inherits(model, "svyglm"))
        {
            result$p <- regTermTest(model, "columns")$p
        }
        else
        {
            f <- regression$summary$fstatistic
            result$p <- 1 - pf(f[1], f[2], f[3])
        }
        results <- list(result)
        names(results)[1] = outcome.name
    }
    else
    {
        # for (i in 1:length(rows))
        #     rows[[i]] <- tidyFactor(rows[[i]])
        # row.variable.levels <- sapply()
    }
    attr(results, "outcome.label") <- outcome.label
    attr(results, "column.names") <- levels(columns)
    attr(results, "compare") <- compare
    attr(results, "output") <- output
    results
}

#' \code{CompareMultipleMeans}
#' Compares means of multiple variables.
#' @param outcome The outcome variable.

#' @export
CompareMultipleMeans <- function(outcomes,
                         columns,
                         Rows = NULL,
                         compare = "Columns",
                         correction = "False Discovery Rate",
                         show.labels = TRUE, ...)
{
    n.outcome.variables <- length(outcomes)
    outcome.names <- names(outcomes)
    results <- list()
    for (i in 1:n.outcome.variables)
        results[[i]] <- CompareMeans(outcomes[[i]], columns, Rows,
                                     compare,
                                     correction,
                                     show.labels,
                                     outcome.name = (outcome.names[i]),
                                     ...)
    attr(results, "column.names") <- attr(results[[1]], "column.names")
    names(results) <- sapply(outcomes, function(x) deparse(substitute(x)))
    class(results) <- c("CompareMultipleMeans", class(results))
    results
}

#' @importFrom flipFormat MeanComparisonsTable
#' @export
print.CompareMultipleMeans <- function(x, ...)
{
    mt <- MeansTables(x)
    mct <- MeanComparisonsTable(
        means = mt$means,
        zs = mt$zs,
        ps = mt$ps,
        r.squared = mt$r.squared,
        overall.p = mt$overall.p,
        column.names = mt$column.names,
        footer = ""
    )
    print(mct)
    invisible(x)
}


extractRowData <- function(row)
{
    coefs <- row$test$coefficients
    k <- length(coefs)
    names(coefs) <- LETTERS[1:k]
    means <- row$grand.mean + coefs
    ps <- row$test$pvalues
    names(ps) <- paste0(LETTERS[1:k], "1")
    return(list(means = means,
           zs = row$test$tstat,
           ps = ps,
           r.squared = row$r.squared,
           overall.p = as.numeric(row$p)))
}

#' \code{CompareMeans}
#' Performs statistical tests comparing means of an outcome variable.
#' @param outcome The outcome variable.
#' @export
MeansTables <- function(x)
{
    means <- NULL
    zs <- NULL
    ps <- NULL
    r.squared <- NULL
    overall.p <- NULL
    for (i in 1:length(x))
    {
        row <- extractRowData(x[[i]][[1]])
        means <- rbind(means, row$means)
        zs <- rbind(zs, row$zs)
        ps <- rbind(ps, row$ps)
        r.squared <- c(r.squared, row$r.squared)
        overall.p <- c(overall.p, row$overall.p)
    }
    rownames(means) <- sapply(x, function(x) attr(x, "outcome.label"))
    column.names <- paste0(attr(x, "column.names"), "<br>","n = ",x[[1]][[1]]$n)
    return(list(means = means,
                zs = zs,
                ps = ps,
                r.squared = r.squared,
                overall.p = overall.p,
                column.names = column.names))
}


#' @importFrom flipTransformations Factor
tidyFactor <- function(x)
{
    x <- Factor(x)
    levels(x)[table(x) == 0] <- NA # Same basic idea as droplevels, except that it retains the attribute 'label'.
    x
}

