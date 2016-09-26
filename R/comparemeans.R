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
#' @param ... Parameters to pass to \code{\link{OneWayANOVA}}.
#' @export
CompareMultipleMeans <- function(outcomes,
                         predictor,
                         title = "",
                         subtitle = "",
                         footer = "",
                         weights = NULL,
                          ...)
{
    if (!is.list(outcomes))
        stop("'outcomes' must be a list or data.frame.")
    n.outcome.outcomes <- length(outcomes)
    result <- list(anovas = MultipleANOVAs(outcomes, predictor, weights = weights, ...),
                   title = title,
                   subtitle = subtitle,
                   footer = footer)
    class(result) <- "CompareMultipleMeans"
    result
}

#' \code{print.CompareMultipleMeans}
#'
#' @param x An object of class \code{CompareMultipleMeans}
#' @param ... Arguments for \code{PrintFormattableANOVAs}.
#' @export
print.CompareMultipleMeans <- function(x, ...)
{
    p <- FormattableANOVAs(x$anovas, x$title, x$subtitle, x$footer, ...)
    print(p)
    invisible(x)
}


