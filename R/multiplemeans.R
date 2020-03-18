#' \code{MultipleMeans}
#'
#' Compares means of multiple outcomes on a categorical predictor.
#'
#' Computes multiple ANOVAs.
#' @param outcomes The outcome variables.
#' @param predictor The factor representing the groups.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param correction The multiple comparison adjustment method: \code{"Table FDR", "Tukey Range", "None",
#' "False Discovery Rate", "Benjamini & Yekutieli", "Bonferroni",
#' "Free Combinations"}, \code{"Hochberg", "Holm",
#' "Hommel", "Single-step"} \code{"Shaffer"}, and \code{"Westfall"}.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance. This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param missing How missing data is to be treated in the ANOVA. Options:
#'   \code{"Error if missing data"}.
#'   \code{"Exclude cases with missing data"}, and
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param p.cutoff The alpha level to be used in testing.
#' @param seed The random number seed used when evaluating the multivariate t-distribution.
#' @param title The title to appear in the output.
#' @param subtitle The footer to appear in the output.
#' @param footer The footer to appear in the output.
#' @param return.data.frame Whether to return a data frame instead of a
#'  formattable widget.
#' @details Conducts multiple \code{OneWayANOVA}s, and puts them in a list. If \code{correction} is
#'  \code{"Table FDR"}, the false discovery rate correction is applied across the entire table. All
#' other corrections are performed within rows. Additional detail about the other parameters can be found in \code{OneWayANOVA}.
#' @importFrom flipFormat Labels
#' @importFrom flipTransformations ProcessQVariables
#' @export
MultipleMeans <- function(outcomes,
                           predictor,
                           subset = NULL,
                           weights = NULL,
                           correction = "Table FDR",
                           robust.se = FALSE,
                           missing = "Exclude cases with missing data",
                           show.labels = FALSE,
                           seed = 1223,
                           p.cutoff = 0.05,
                          title = "",
                          subtitle = "",
                          footer = "",
                          return.data.frame = FALSE)
{
    outcomes <- ProcessQVariables(outcomes)
    predictor <- ProcessQVariables(predictor)

    anovas <- MultipleANOVAs(outcomes,
                        predictor,
                        subset = subset,
                        weights = weights,
                        robust.se = robust.se,
                        correction = correction,
                        show.labels = show.labels,
                        return.all = TRUE)

    if (return.data.frame)
    {
        output <- ANOVAsAsTable(anovas)
        result <- cbind(output$means, R_Squared = output$r.squared, p = output$overall.p)
        colnames(result) <- gsub("\n", " ", colnames(result))
        result
    }
    else
        FormattableANOVAs(anovas,
                          title = title,
                          subtitle = subtitle,
                          footer = footer)
}
