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
    # for (i in seq_along(outcomes))
    # {
    #     if (is.null(attr(outcomes[[i]], "name")))
    #         attr(outcomes[[i]], "name") <- Names(outcomes[[i]])
    #     if (is.null(attr(outcomes[[i]], "label")))
    #         attr(outcomes[[i]], "label") <- Labels(outcomes[[i]])
    #
    # }
    # print(Labels(outcomes))
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
  #
    # if (!is.list(outcomes))
    #     stop("'outcomes' must be a list or data.frame.")
    # # n.outcome.outcomes <- length(outcomes)
    # # result <- list(anovas = MultipleANOVAs(outcomes, predictor, weights = weights, ...),
    # #                title = title,
    # #                subtitle = subtitle,
    # #                footer = footer,
    # #                return.all = TRUE)
    #
    #
    #
    #     n.outcome.outcomes <- length(outcomes)
    # result <- list(anovas = MultipleANOVAs(outcomes, predictor, weights = weights, ...),
    #                title = title,
    #                subtitle = subtitle,
    #                footer = footer,
    #                return.all = TRUE)
    #
    #

#    class(result) <- "CompareMultipleMeans"
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

