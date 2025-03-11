#' \code{TableOfMeans}
#'
#' Compares mean of one variable by two factors.
#'
#' @param outcome The outcome variable.
#' @param row The predictor to show in the rows. Used to filter the data.
#' @param column The predictor to show in the columns.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param correction The false discovery rate correction to be applied when computing the corrected p-values.
#' Defaults to applying the False Discovery Rate correction to all the cells in the table (\code{"Table FDR"}). See \code{\link{OneWayANOVA}}
#' for other options.
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance. This parameter is ignored
#'   if weights are applied (as weights already employ a sandwich estimator).
#' @param show.labels Show labels instead of variable names
#' @param p.cutoff The alpha level to be used in testing.
#' @param ... Parameters to pass to \code{\link{OneWayANOVA}}.
#' @importFrom flipFormat Labels Names
#' @importFrom flipTransformations FactorToIndicators AsNumeric ProcessQVariables
#' @export
TableOfMeans <- function(outcome,
                         row,
                         column,
                         subset = NULL,
                         weights = NULL,
                         correction = "Table FDR",
                         robust.se = FALSE,
                         show.labels = TRUE,
                         p.cutoff = 0.05,
                         ...)
{
    validateInputs(outcome, row, column, subset, weights)

    # Removing missing values and filtering weights.
    df <- prepareData(outcome, row, column, subset, weights, FALSE, "Exclude cases with missing data")
    weights <- if (is.null(weights)) NULL else df[, 4]

    # if (robust.se != FALSE)
    #     footer <- paste0(footer, "heteroscedastic robust standard errors (", if(robust.se == TRUE) "hc3" else robust.se, ");")
    row.variable <- df[, 2]
    outcomes.by.rows <- FactorToIndicators(row.variable)
    outcomes.by.rows[outcomes.by.rows == 0] <- NA
    outcomes.by.rows <- sweep(outcomes.by.rows, 1, AsNumeric(df[, 1], binary = FALSE), "*")
    counts <- table(row.variable)
    for (i in seq_along(outcomes.by.rows))
    {
        attr(outcomes.by.rows[, i], "label") <- paste0(levels(row.variable)[i], " n: ", counts[i])
        attr(outcomes.by.rows[, i], "name") <- paste0(Names(row), i)
    }
    columns.with.data <- apply(!is.na(outcomes.by.rows), 2, sum) > 0
    outcomes.by.rows <- outcomes.by.rows[, columns.with.data, drop = FALSE]
    # anovas <- MultipleANOVAs(outcomes.by.rows,
    #                     df[, 3],
    #                     subset = NULL,
    #                     weights = weights,
    #                     #compare = "To mean",
    #                     robust.se = robust.se,
    #                     correction = correction,
    #                     #alernative = "Two-sided",
    #                     show.labels = show.labels,
    #                     return.all = TRUE)
    # #ps <- attr(result$anovas, "ps")
    # outcome.title <- if(show.labels) Labels(outcome) else Names(outcome)
    # row.title <- if(show.labels) Labels(row) else Names(row)
    # column.title <- if(show.labels) Labels(column) else Names(column)
    # FormattableANOVAs(anovas,
    #                 title = paste0("Mean of ", outcome.title, " by ", row.title, " and ", column.title),
    #                 subtitle = paste0("Rows: ", row.title, "; Columns:", column.title),
    #                 footer = footer)
    outcome.title <- if(show.labels) Labels(outcome) else Names(outcome)
    row.title <- if(show.labels) Labels(row) else Names(row)
    column.title <- if(show.labels) Labels(column) else Names(column)

    MultipleMeans(outcomes.by.rows,
                           df[, 3],
                           subset = NULL,
                           weights = weights,
                           correction = correction,
                           robust.se = robust.se ,
                           show.labels = show.labels,
                           p.cutoff = p.cutoff,
                           title =  paste0("Mean of ", outcome.title, " by ", row.title, " and ", column.title),
                           subtitle = paste0("Rows: ", row.title, "; Columns: ", column.title),
                           footer = attr(df, "footer"))
}

#' @importFrom flipData CleanWeights CleanSubset
#' @importFrom flipU StopForUserError
validateInputs <- function(outcome, row, column, subset, weight)
{
    n <- length(outcome)
    .checkLengths <- function(x.len, y.len, x.ctrl.name, y.ctrl.name)
    {
        if (x.len != y.len)
        {
            err.msg <- paste0("The variables in %s and %s are different lengths.",
                              " Please check that they come from the same dataset.")

            StopForUserError(gettextf(err.msg, x.ctrl.name, y.ctrl.name), call. = FALSE)
        }
    }
    .checkLengths(n, length(row), "Outcome", "Rows")
    .checkLengths(n, length(column), "Outcome", "Columns")
    if (length(subset) > 1)
        .checkLengths(n, length(subset), "Outcome", "Filter(s)")
    subset <- CleanSubset(subset, n)
    if (!is.null(weight))
    {
        weight <- CleanWeights(weight)
        .checkLengths(n, length(weight), "Outcome", "Weight")
        subset[weight == 0] <- FALSE
    }
    outcome <- outcome[subset]
    row <- row[subset]
    column <- column[subset]

    .checkForConstantVariable <- function(x, gui.ctrl.name)
    {
        ## We allow a factor in Rows may be subset to a single value
        ## See test-tableofmeans.R#L10
        if (is.factor(x) && gui.ctrl.name == "Rows")
            return()
        if (all(duplicated(x)[-1L]))
        {
            err.msg <- paste0("All values in %s are identical.",
                          " Please check your inputs are correct.")
            StopForUserError(gettextf(err.msg, gui.ctrl.name), call. = FALSE)
        }
    }
    .checkForConstantVariable(outcome, "Outcome")
    .checkForConstantVariable(row, "Rows")
    .checkForConstantVariable(column, "Columns")

    .checkAllValuesEqual <- function(x, y, x.ctrl.name, y.ctrl.name)
    {
        if (isTRUE(all.equal(x, y)))
        {
            err.msg <- paste0("The variables in %s and %s contain identical values.",
                          " Please check your inputs are correct.")
            StopForUserError(gettextf(err.msg, x.ctrl.name, y.ctrl.name), call. = FALSE)
        }
    }
    .checkAllValuesEqual(outcome, row, "Outcome", "Rows")
    .checkAllValuesEqual(outcome, column, "Outcome", "Columns")
    .checkAllValuesEqual(row, column, "Rows", "Columns")

    return()
}
