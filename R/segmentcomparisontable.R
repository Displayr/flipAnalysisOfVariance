#' \code{SegmentComparisonTable}
#'
#' Creates a html table allowing comparison between segments using
#' variables or variable sets
#' @param x A list of variables or variable sets. These can be numeric or
#'    categorical (PickOne or PickAny). It is expected that the attributes
#'    for "question", "label" and "questiontype" is defined.
#' @param group The segmentation variable. Should be a factor.
#' @param weights Numeric; An optional vector of sampling weights. 
#'     Should be the same length as \code{group}.
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param format.numeric.decimals The number of decimals shown in the output table
#'     for numeric values.
#' @param format.percentage.decimals The number of decimals shown in the output table
#'     for percentage values.
#' @param format.conditional.fill Whether the fill color of the cells should reflect the
#'     value in the cells.
#' @param format.numeric.fill.colors A vector or comma-separated list of 5 colors that
#'     is used when \code{format.conditional.fill}. The colors indicate that 
#'     the standardized number value in the cell is smaller than -0.2, -0.1, 0, 0.1, 0.2.
#'     Numeric variables are standardized by dividing twice the standard deviation
#'     http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf
#' @param format.percentage.fill.colors A vector or comma-separated list of 5 colors that
#'     is used when \code{format.conditional.fill}. The colors indicate that 
#'     the percentage in the cell is smaller than -20\%, -10\%, 0\%, 10\%, 20\%.
#' @param show.index.values Percentage values are shown as a ratio to the percentage
#'     computed on the whole population (i.e. unsegmented).
#' @param cell.fill The default background color of the cells in the table.
#' @param summary.cell.fill The background color of the first two rows in
#'     the table which gives a summary of the segmentation variable.
#' @param font.color Default font color of the cells in the table.
#' @param font.size Default font size.
#' @param font.unit One of "px" or "pt".
#' @param row.height Height of one row in the table (includes units).
#' @param font.color.set.if.nonsignificant Logical; whether p-values should be computed
#'     to test the significance of the value in the table.
#' @param font.color.nonsignificant Font color used to show insignificant values if
#'     \code{font.color.set.if.nonsignificant}.
#' @param font.color.confidence The confidence level below which insignificant values
#'     should be shown in \code{font.color.nonsignificant} if
#'     \code{font.color.set.if.nonsignficant}.
#' @param font.color.FDRcorrection Logical; whether an FDR correction is applied to deal
#'    with multiple testing.
#' @param show.question.name Whether the question name should be shown in the output table.
#' @param col.widths Vector or comma-separated list to control column widths.
#' @param row.header.font.weight One of "bold" or "normal".
#' @param row.span.font.weight One of "bold" or "normal".
#' @param col.header.font.weight One of "bold" or "normal".
#' @param row.header.pad Space between border and text.
#' @param row.span.pad Space between border and text.
#' @param row.header.fill Background color of row header cells.
#' @param col.header.fill Background color of column header cells.
#' @param row.span.fill Background color of row span cells.
#' @param corner.fill Background color of corner cells.
#' @param ... Other parameters passed to \link[flipFormat]{CreateCustomTable}.
#' @importFrom flipFormat CreateCustomTable
#' @importFrom flipU ConvertCommaSeparatedStringToVector CopyAttributes
#' @importFrom flipStatistics WeightedTable Table StatisticsByGroup Mean StandardDeviation
#' @importFrom flipRegression PValueAdjustFDR
#' @importFrom flipTransformations AsDataFrame
SegmentComparisonTable <- function(x, group, weights = NULL, subset = TRUE,
                                   format.numeric.decimals = 1,
                                   format.percentage.decimals = 0,
                                   format.conditional.fill = TRUE,
                                   format.numeric.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",
                                   format.percentage.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",
                                   show.index.values = FALSE, 
                                   cell.fill = "#FFFFFF",
                                   font.color = "#2C2C2C", 
                                   font.size = 10,
                                   font.unit = "px",
                                   row.height = paste0(font.size * 1.1, font.unit),
                                   font.color.set.if.nonsignificant = TRUE,
                                   font.color.nonsignificant = "#CCCCCC", 
                                   font.color.confidence = 0.95,
                                   font.color.FDRcorrection = FALSE,
                                   show.question.name = TRUE,
                                   col.widths = "100px, 100px",
                                   row.header.font.weight = "normal",
                                   row.span.font.weight = "normal",
                                   col.header.font.weight = "normal",
                                   row.header.pad = "5px",
                                   row.span.pad = "5px",
                                   summary.cell.fill = "#EFEFEF",
                                   row.header.fill = "#DDDDDD",
                                   row.span.fill = row.header.fill,
                                   col.header.fill = row.header.fill,
                                   corner.fill = row.header.fill,
                                   ...)
{

    format.percentage.fill.colors <- ConvertCommaSeparatedStringToVector(format.percentage.fill.colors)
    if (length(subset) > 1)
    {
        group <- group[subset]
        if (length(weights) > 1)
            weights <- weights[subset]
    }

    counts <-t(WeightedTable(group, weights = weights))
    result <- rbind(counts, counts/sum(counts))
    row.labels <- c("Sample size", "Percentage")
    group.label <- attr(group, "label") # group should be a variable not a variable set
    if (is.null(group.label))
        group.label <- " "
    row.span <- list(list(label = group.label, height = 2))
    row.format <- c("numeric", "percentage")
    row.vvi <- c(0, 0)
    row.vcol <- c(0, 0)

    v.list <- ProcessQVariables(x)
    for (vvi in 1:length(v.list))
    {
        vv <- v.list[[vvi]]
        if (length(dim(vv)) < 2)
            vv <- CopyAttributes(vv[subset], vv)
        else
            vv <- CopyAttributes(vv[,subset], vv)
        
        if (isTRUE(attr(vv, "questiontype") %in% c("NumberMulti")))
        {
            tmp <- t(StatisticsByGroup(vv, group = group, weights = weights))
        } else
            tmp <- crosstabOneVariable(vv, group = group, weights = weights)

        tmp.nrow <- 1
        if (length(dim(tmp)) < 2 || NROW(tmp) == 1)
        {
            row.span[[length(row.span) + 1]] <- list(label = attr(vv, "question"), height = 1)
            row.labels <- c(row.labels, attr(vv, "label"))
            row.vcol <- c(row.vcol, 0)
        } else
        {
            tmp.nrow <- nrow(tmp)
            row.vcol <- c(row.vcol, 1:tmp.nrow)
            row.span[[length(row.span) + 1]] <- list(label = attr(vv, "question"), height = nrow(tmp))
            if (!is.null(rownames(tmp)))
                row.labels <- c(row.labels, rownames(tmp))
        }
        tmp.numeric <- isTRUE(attr(vv, "questiontype") %in% c("NumberMulti", "Number"))
        row.format <- c(row.format, rep(if (tmp.numeric) "numeric" else "percentage", tmp.nrow))
        row.vvi <- c(row.vvi, rep(vvi, tmp.nrow))

        # Compute column percentages for Pick One questions
        if (isTRUE(attr(vv, "questiontype") == "PickOne"))
        {
            tmp <- sweep(tmp, 2, colSums(tmp), "/")
            v.list[[vvi]] <- AsDataFrame(vv, categorical.as.binary = TRUE)
        } else
            v.list[[vvi]] <- vv # save subsetted variable
        
        # Compute Index - does not affect font color or cell fill
        if (show.index.values && !tmp.numeric)
        {
            tot.mean <- Mean(v.list[[vvi]], weights = weights)
            if (NROW(tmp) > 1)
                tmp <- sweep(tmp, 1, tot.mean, "/")
            else
                tmp <- tmp/tot.mean
        }
        result <- rbind(result, tmp)
    }

    rownames(result) <- NULL # result is preserved in numeric form for exporting
    result.formatted <- matrix("", nrow(result), ncol(result))
    for (i in 1:nrow(result))
        result.formatted[i,] <- if (row.format[i] == "numeric") formatC(result[i,], format.numeric.decimals, format = "f")
                                else                            paste0(formatC(result[i,] * 100, format.percentage.decimals, format = "f"), "%")
    result.formatted[!is.finite(result)] <- ""
    result.formatted <- formatC(result.formatted, format = "s", width = max(nchar(result.formatted)))
    result.formatted <- gsub(" ", "&nbsp;", result.formatted)
    colnames(result.formatted) <- colnames(result)

    # Font color is determined by p-values
    results.font.color = font.color
    if (font.color.set.if.nonsignificant)
    {
        results.font.color <- matrix(font.color, nrow(result), ncol(result))
        results.font.color[3:nrow(result),] <- font.color.nonsignificant # include p = NA 
        pvals <- matrix(NA, nrow(result), ncol(result))
        rownames(pvals) <- row.labels
        colnames(pvals) <- colnames(result)
        for (i in 3:nrow(result))
        {
            tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                       else                  v.list[[row.vvi[i]]][,row.vcol[i]]
            pvals[i,] <- PValsByGroup(tmp.var, group, weights, is.binary = row.format[i] != "numeric")
        }
        if (font.color.FDRcorrection)
            pvals <- PValueAdjustFDR(pvals, alpha = 1 - font.color.confidence) 
        print(pvals)
        results.font.color[which(pvals < 1 - font.color.confidence)] <- font.color
    }

    # Fill color is determined by cell values
    cell.fill <- matrix(cell.fill, nrow(result), ncol(result))
    cell.fill[1:2,] <- summary.cell.fill
    if (format.conditional.fill)
    {
        for (i in 3:nrow(result))
        {
            if (row.format[i] == "percentage") # and if show index values is used
            {
                cell.fill[i,which(result[i,] <  0.2)] <- format.percentage.fill.colors[4]
                cell.fill[i,which(result[i,] <  0.1)] <- format.percentage.fill.colors[3]
                cell.fill[i,which(result[i,] < -0.1)] <- format.percentage.fill.colors[2]
                cell.fill[i,which(result[i,] < -0.2)] <- format.percentage.fill.colors[1]
            } else
            {
                tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                           else                  v.list[[row.vvi[i]]][,row.vcol[i]]
                tmp.sd <- StandardDeviation(tmp.var, weights = weights)
                tmp.gmean <- StatisticsByGroup(tmp.var/(2*tmp.sd), group = group, weights = weights)

                cell.fill[i,which(tmp.gmean <  0.2)] <- format.percentage.fill.colors[4]
                cell.fill[i,which(tmp.gmean <  0.1)] <- format.percentage.fill.colors[3]
                cell.fill[i,which(tmp.gmean < -0.1)] <- format.percentage.fill.colors[2]
                cell.fill[i,which(tmp.gmean < -0.2)] <- format.percentage.fill.colors[1]

            }
        }
    }

    if (!show.question.name)
        row.labels <- FALSE
    output <- CreateCustomTable(result.formatted, row.header.labels = row.labels, 
                      row.spans = row.span, cell.fill = cell.fill, 
                      cell.font.color = results.font.color,
                      row.span.fill = row.span.fill, row.header.fill = row.header.fill, 
                      corner.fill = corner.fill, col.header.fill = col.header.fill,
                      font.unit = font.unit, font.size = font.size, col.widths = col.widths,
                      row.span.pad = row.span.pad, row.header.pad = row.header.pad, 
                      row.header.font.weight = row.header.font.weight,
                      row.span.font.weight = row.span.font.weight,
                      col.header.font.weight = col.header.font.weight, 
                     ...)
    result.rows <- unlist(sapply(row.span, function(r) rep(r$label, r$height)))
    rownames(result) <- paste0(result.rows, ":", row.labels)
    attr(output, "ChartData") <- result
    return(output)
}


crosstabOneVariable <- function(x, group, weights = NULL, subset = TRUE,
        categorical.as.binary = TRUE)
{
    data <- data.frame(x = x, y = group)
    data$w <- if (is.null(weights)) rep.int(1L, NROW(data)) else weights
    if (length(subset) > 1)
        data <- data[subset,]

    if (is.numeric(x) || !categorical.as.binary)
    {
        data$x <- AsNumeric(data$x, binary = FALSE)
        if (!is.null(weights))
        {
            data$xw <- data$x * weights
            out <- Table(xw~y, data = data, FUN = sum)/Table(w~y, data = data, FUN = sum)

        } else
            out <- Table(x~y, data = data, FUN = mean)
    } else
    {
        out <- Table(w~x+y, data = data, FUN = sum)
    }
    return(out)
}


