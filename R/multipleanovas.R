    #' \code{MultipleANOVAs}
#'
#' Computes multiple ANOVAs.
#' @param dependents The outcome variables.
#' @param independent The factor representing the groups.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param compare One of \code{"To mean", "Pairwise", "To first"} (which implement's Dunnett's C, when
#' combined with 'correction' == 'Tukey Range'), or \code{"All"}
#' @param correction The multiple comparison adjustment method: \code{"Table FDR", "Tukey Range", "None",
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
#' @param ... Other parameters to be passed to \code{OneWayANOVA}.
#' @details Conducts multiple \code{OneWayANOVA}s, and puts them in a list. If \code{correction} is
#'  \code{"Table FDR"}, the false discovery rate correction is applied across the entire table. All
#' other corrections are performed within rows. Additional detail about the other parameters can be found in \code{OneWayANOVA}.
#' @importFrom flipFormat Labels
#' @export
MultipleANOVAs <- function(dependents,
                           independent,
                           subset = NULL,
                           weights = NULL,
                           compare = "To mean",
                           correction = "Table FDR",
                           robust.se = FALSE,
                           alternative = "Two-sided",
                           missing = "Exclude cases with missing data",
                           show.labels = FALSE,
                           seed = 1223,
                           p.cutoff = 0.05,
                           ...)
{
    anovas <- suppressWarnings(lapply(dependents, function(x) OneWayANOVA(x,
                                                       independent,
                                                       compare = "To mean",
                                                       subset = subset,
                                                       weights = weights,
                                                       correction = if(correction == "Table FDR") "None" else correction,
                                                       robust.se = robust.se,
                                                       #alternative = alternative,
                                                       #p.cutoff = p.cutoff,
                                                       #seed = seed,
                                                       #return.all = TRUE,
                                                       ...
                                                       )))
    # Performing FDR correction.
    ps <- unlist(lapply(anovas, function(x) x$coefs[, 4]))
    if (correction == "Table FDR")
    {
        n.anovas <- length(anovas)
        ps <- p.adjust(ps, method = "fdr")
        counter <- 1
        for (a in 1:n.anovas)
        {
            k <- nrow(anovas[[a]]$coefs)
            var.ps <- ps[counter:(counter + k - 1)]
            anovas[[a]]$coefs[, 4] <- var.ps
            anovas[[a]]$p <- min(var.ps)
            counter <- counter + k
        }
    }
    attr(anovas, "ps") <- ps
    names(anovas) <- flipFormat::Labels(dependents)
    anovas
}


#' FormattableANOVAs
#'
#' @param anovas List of OneWayANOVA objects.
#' @param title String showing the title of the table.
#' @param subtitle String showing the subtitle of the table.
#' @param footer String showing the title of the table.
#' @importFrom flipFormat MeanComparisonsTable
#' @export
FormattableANOVAs <- function(anovas, title, subtitle, footer)
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
        footer = footer,
        p.cutoff = anovas[[1]]$p.cutoff)
    return(mct)
}

#' \code{ANOVAsAsTable}
#' Converts a list of ANOVAs into a format that can be prettily formatted.
#' @param x The list of ANOVAs.
#' @importFrom flipTables Rbind
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
        means <- Rbind(means, coefs[, 1])
        zs <- Rbind(zs, coefs[, 3])
        ps <- Rbind(ps, coefs[, 4])
        r.squared <- c(r.squared, i$r.squared)
        overall.p <- c(overall.p, i$p)
    }
    rownames(means) <- names(x)
    group.names <- x[[1]]$column.names
    n <- x[[1]]$n
    column.names <- paste0(group.names, "<br>","n: ", n)
    return(list(means = means,
                zs = zs,
                ps = ps,
                r.squared = r.squared,
                overall.p = overall.p,
                column.names = column.names))
}

