#' \code{CompareMultipleMeans}
#'
#' Compares means of multiple variables.
#'
#' @param outcomes The outcome variables.
#' @param predictor The factor indicating group membership.
#' @param title Main title for the table.
#' @param subtitle Subtitle for the table.
#' @param footer Footer of the table.
#' @param weights The sampling weights
#' @param show.labels Show labels instead of variable names
#' @param ... Parameters to pass to \code{\link{OneWayANOVA}}.
#' @importFrom flipFormat Labels Names
#' @importFrom flipTransformations ProcessQVariables
#' @export
CompareMultipleMeans <- function(outcomes,
                         predictor,
                         title = "",
                         subtitle = "",
                         footer = "",
                         weights = NULL,
                         show.labels = TRUE,
                          ...)
{
    outcomes <- ProcessQVariables(outcomes)
    predictor <- ProcessQVariables(predictor)

    manova <- OneWayMANOVA(outcomes,
                        predictor,
                        subset = NULL,
                        return.all = TRUE,
                        show.labels = show.labels,
                        ...)
    result <- FormattableANOVAs(manova$anovas,
                    title = title,
                    subtitle = subtitle,
                    footer = footer)
    result
}
#'
#' #' \code{print.CompareMultipleMeans}
#' #'
#' #' @param x An object of class \code{CompareMultipleMeans}
#' #' @param ... Arguments for \code{PrintFormattableANOVAs}.
#' #' @export
#' print.CompareMultipleMeans <- function(x, ...)
#' {
#'     print(table)
#' }
#'

