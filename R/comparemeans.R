#' \code{MultipleANOVAs}
#'
#' Computes multiple ANOVAs.
#' @param outcomes The outcome variables, as a data frame.
#' @param groups The factor representing the groups.
#' @param rows The list of factors representing the rows.
#' @param correction The multiple comparison adjustment method: \code{"None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"} (Westfall et al. 1999), \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} (Bretz et al. 2010) \code{"Shaffer"}, and \code{"Westfall"}.
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
#' @importFrom flipFormat Labels
#' @export
MultipleANOVAs <- function(outcomes,
                        groups,
                        subset = NULL,
                        weights = NULL,
                        compare = "Pairwise",
                        correction = "False Discovery Rate",
                        show.labels = TRUE,
                        output = "Pretty",
                        outcome.name = NULL,
                        p.cutoff = 0.05,
                        ...)
{
    anovas <- lapply(outcomes, function(x) OneWayANOVA(x, groups, compare = "To mean", weights = weights, ...))
    if (is.data.frame(outcomes))
        names(anovas) <- flipFormat::Labels(outcomes)
    else
    {
        nms <- names(outcomes)
        if (is.null(nms))
            paste("Variable", 1:length(anovas))
    }
    anovas
}


#' \code{CompareMultipleMeans}
#' Compares means of multiple variables.
#' @param outcome The outcome variable.
#' @export
CompareMultipleMeans <- function(variables,
                         groups,
                         title = "",
                         subtitle = "",
                         footer = "",
                         weights = NULL,
                          ...)
{
    if (!is.list(variables))
        stop("'variables' must be a list or data.frame.")
    n.outcome.variables <- length(variables)
    result <- list(anovas = MultipleANOVAs(variables, groups, weights = weights, ...),
                   title = title,
                   subtitle = subtitle,
                   footer = footer)
    class(result) <- "CompareMultipleMeans"
    result
}

#' @importFrom flipFormat MeanComparisonsTable
#' @export
print.CompareMultipleMeans <- function(x, ...)
{
    printFormattableANOVAs(x$anovas, x$title, x$subtitle, x$footer, ...)
    invisible(x)
}
#
#
# extractRowData <- function(row)
# {
#     coefs <- row$test$coefficients
#     k <- length(coefs)
#     names(coefs) <- LETTERS[1:k]
#     means <- row$grand.mean + coefs
#     ps <- row$test$pvalues
#     names(ps) <- paste0(LETTERS[1:k], "1")
#     return(list(means = means,
#            zs = row$test$tstat,
#            ps = ps,
#            r.squared = row$r.squared,
#            overall.p = as.numeric(row$p)))
# }

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



