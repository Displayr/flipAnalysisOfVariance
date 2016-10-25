#' \code{TableOfMeans}
#'
#' Compares mean of one variable by two factors.
#'
#' @param outcome The outcome variable.
#' @param row The predictor to show in the rows. Used to filter the data.
#' @param column The predictor to show in the columns.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance. This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param show.labels Show labels instead of variable names
#' @param ... Parameters to pass to \code{\link{OneWayANOVA}}.
#' @importFrom flipFormat Labels Names
#' @importFrom flipTransformations FactorToIndicators AsNumeric
#' @export
TableOfMeans <- function(outcome,
                         row,
                         column,
                         correction = "Table FDR",
                         robust.se = FALSE,
                         weights = NULL,
                         show.labels = TRUE,
                         ...)
{
#    predictor.label <- if(show.labels) Labels(predictor) else OriginalName(predictor)
    # Removing missing values and filtering weights.
    df <- prepareData(outcome, row, column, subset, weights, FALSE, "Exclude cases with missing data")
    weights <- if (is.null(weights)) NULL else df[, 4]

    footer <- attr(df, "footer")
    row.variable <- df[, 2]
    outcomes.by.rows <- FactorToIndicators(row.variable)
    outcomes.by.rows[outcomes.by.rows == 0] <- NA
    outcomes.by.rows <- sweep(outcomes.by.rows, 1, AsNumeric(df[, 1], binary = FALSE), "*")
    counts <- table(row.variable)
    for (i in seq_along(outcomes.by.rows))
    {
        attr(outcomes.by.rows[, i], "label") <- paste0(levels(row)[i], " n: ", counts[i])
        attr(outcomes.by.rows[, i], "name") <- paste0(Names(row), i)
    }
    anovas <- MultipleANOVAs(outcomes.by.rows,
                        df[, 3],
                        subset = NULL,
                        weights = weights,
                        #compare = "To mean",
                        robust.se = robust.se,
                        correction = correction,
                        #alernative = "Two-sided",
                        show.labels = show.labels,
                        p.cutoff = p.cutoff,
                        seed = seed,
                        return.all = TRUE)
    #ps <- attr(result$anovas, "ps")
    outcome.title <- if(show.labels) Labels(outcome) else Names(outcome)
    row.title <- if(show.labels) Labels(row) else Names(row)
    column.title <- if(show.labels) Labels(column) else Names(column)
    FormattableANOVAs(anovas,
                    title = paste0("Mean of ", outcome.title, " by ", row.title, " and ", column.title),
                    subtitle = paste0("Rows: ", row.title, "; Columns:", column.title),
                    footer = footer)
}
