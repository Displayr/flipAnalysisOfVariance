#' \code{SegmentComparisonTable}
#'
#' Creates a html table allowing comparison between segments using
#' variables or variable sets
#' @param x A list of variables or variable sets. These can be numeric or
#'    categorical (PickOne or PickAny). It is expected that the attributes
#'    for "question", "label" and "questiontype" is defined.
#' @param group The segmentation variable. Should be a factor.
#' @param remove.net.or.sum Remove NET or SUM rows for Pick Any or NumberMulti variables.
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
#' @param font.color.nonparametric Logical; whether a non-parametric test should 
#     be used.
#' @param show.question.name Whether the question name should be shown in the output table.
#' @param col.widths Vector or comma-separated list to control column widths.
#' @param row.header.font.weight One of "bold" or "normal".
#' @param row.span.font.weight One of "bold" or "normal".
#' @param col.header.font.weight One of "bold" or "normal".
#' @param row.header.pad Numeric; Space between border and text in pixels.
#' @param row.span.pad Numeric; Space between border and text in pixels.
#' @param row.header.fill Background color of row header cells.
#' @param col.header.fill Background color of column header cells.
#' @param row.span.fill Background color of row span cells.
#' @param corner.fill Background color of corner cells.
#' @param ... Other parameters passed to \link[flipFormat]{CreateCustomTable}.
#' @importFrom flipFormat CreateCustomTable
#' @importFrom flipU ConvertCommaSeparatedStringToVector CopyAttributes
#' @importFrom flipStatistics WeightedTable Table StatisticsByGroup Mean StandardDeviation
#' @importFrom flipRegression PValueAdjustFDR
#' @importFrom flipTransformations AsDataFrame FactorToNumeric
#' @export
SegmentComparisonTable <- function(x, group, weights = NULL, subset = TRUE,
                                   remove.net.or.sum = FALSE,
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
                                   row.height = paste0(font.size * 1.5, font.unit),
                                   font.color.set.if.nonsignificant = TRUE,
                                   font.color.nonsignificant = "#CCCCCC", 
                                   font.color.confidence = 0.95,
                                   font.color.FDRcorrection = FALSE,
                                   font.color.nonparametric = FALSE,
                                   show.question.name = TRUE,
                                   col.widths = "100px, 100px",
                                   row.header.font.weight = "normal",
                                   row.span.font.weight = "normal",
                                   col.header.font.weight = "normal",
                                   row.header.pad = 2,
                                   row.span.pad = row.header.pad,
                                   summary.cell.fill = "#EFEFEF",
                                   row.header.fill = "#DDDDDD",
                                   row.span.fill = row.header.fill,
                                   col.header.fill = row.header.fill,
                                   corner.fill = row.header.fill,
                                   ...)
{
    group.label <- attr(group, "label") # group should be a variable not a variable set
    if (is.null(group.label))
        group.label <- " "
    corner.text <- ""

    group <- ProcessQVariables(group)
    format.percentage.fill.colors <- ConvertCommaSeparatedStringToVector(format.percentage.fill.colors)
    format.numeric.fill.colors <- ConvertCommaSeparatedStringToVector(format.numeric.fill.colors)
    if (length(subset) > 1)
    {
        group <- group[subset]
        if (length(weights) > 1)
            weights <- weights[subset]
    }

    counts <-t(WeightedTable(group, weights = weights))
    result <- rbind(counts, counts/sum(counts))
    row.labels <- c("Sample size", "Percentage")
    if (length(weights) > 1)
    {
        row.labels <- paste(row.labels, c("(unweighted)", "(weighted)"))
        result[1,] <- table(group)
    }

    row.span <- list(list(label = "Segment size", height = 2))
    row.span <- list(list(label = group.label, height = 2))
    row.format <- c("numeric", "percentage")
    row.vvi <- c(0, 0)
    row.vcol <- c(0, 0)

    v.list <- list()
    index.values <- list()
    for (vvi in 1:length(x))
    {
        # Convert everything to a data frame
        v.qtype <- paste0("", attr(x[[vvi]], "questiontype")) # avoid NULL
        if (v.qtype == "Date")
            vv <- FactorToNumeric(ProcessQVariables(x[[vvi]]), binary = TRUE, remove.first = FALSE)
        else
            vv <- AsDataFrame(x[[vvi]], categorical.as.binary = TRUE)
        if (length(subset) > 1)
            vv <- vv[subset,,drop = FALSE]
        tmp.colnames <- colnames(x[[vvi]])
        if (remove.net.or.sum)
        {
            net.ind <- which(colnames(vv) %in% c("NET", "SUM", "Total"))
            if (length(net.ind) > 0)
            {
                vv <- vv[, -net.ind, drop = FALSE]
                tmp.colnames <- tmp.colnames[-net.ind]
            }
        }

        # Compute main statistic (average/percentage)
        tmp <- t(StatisticsByGroup(vv, group = group, weights = weights))
        if (v.qtype %in% c("PickOne", "Date"))
            tmp <- sweep(tmp, 2, colSums(tmp), "/")

        # Use question attributes to determine row names
        if (v.qtype == "Date")
            rownames(tmp) <- levels(attr(x[[vvi]], "QDate"))
        else if (v.qtype == "PickOne")
            rownames(tmp) <- levels(x[[vvi]])
        else if (v.qtype %in% c("PickAny", "NumberMulti") && ncol(vv) > 1)
            rownames(tmp) <- tmp.colnames
        else
            rownames(tmp) <- attr(x[[vvi]], "label")

        tmp.nvar <- ncol(vv)
        row.vcol <- c(row.vcol, 1:tmp.nvar)
        row.span[[length(row.span) + 1]] <- list(label = attr(x[[vvi]], "question"), height = tmp.nvar)
        if (!is.null(rownames(tmp)))
            row.labels <- c(row.labels, rownames(tmp))
        tmp.numeric <- isTRUE(attr(x[[vvi]], "questiontype") %in% c("NumberMulti", "Number"))
        row.format <- c(row.format, rep(if (tmp.numeric) "numeric" else "percentage", tmp.nvar))
        row.vvi <- c(row.vvi, rep(vvi, tmp.nvar))
        v.list[[vvi]] <- vv # save subsetted variable
        
        # Always compute index values because they are used for cell fill
        if (!tmp.numeric)
        {
            ind.not.missing <- !is.na(group)
            tot.mean <- Mean(vv[ind.not.missing,,drop = FALSE], weights = weights[ind.not.missing])
            if (NROW(tmp) > 1)
                index.values[[vvi]] <- sweep(tmp, 1, tot.mean, "/")
            else
                index.values[[vvi]] <- tmp/tot.mean
            
            if (show.index.values)
                tmp <- index.values[[vvi]]
        }
        result <- rbind(result, tmp)
    }

    rownames(result) <- NULL # result is preserved in numeric form for exporting
    result.formatted <- matrix("", nrow(result), ncol(result))
    for (i in 1:nrow(result))
        result.formatted[i,] <- if (row.format[i] == "numeric") formatC(result[i,], if (i == 1) 0 else format.numeric.decimals, format = "f", big.mark = ",")
                                else                            paste0(formatC(result[i,] * 100, format.percentage.decimals, format = "f", big.mark = ","), "%")
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
            pvals[i,] <- pvalsByGroup(tmp.var, group, weights, is.binary = row.format[i] != "numeric",
                                      non.parametric = font.color.nonparametric)
        }
        if (font.color.FDRcorrection)
            pvals <- PValueAdjustFDR(pvals, alpha = 1 - font.color.confidence) 
        results.font.color[which(pvals < 1 - font.color.confidence)] <- font.color
    }

    # Fill color is determined by cell value
    cell.fill <- matrix(cell.fill, nrow(result), ncol(result))
    cell.fill[1:2,] <- summary.cell.fill
    if (format.conditional.fill)
    {
        for (i in 3:nrow(result))
        {
            if (row.format[i] == "percentage")
            {
                f.vals <- as.numeric((index.values[[row.vvi[i]]])[row.vcol[i],] - 1.0)
                f.cols <- format.numeric.fill.colors
            } else
            {
                tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                           else                  v.list[[row.vvi[i]]][,row.vcol[i]]
                tmp.sd <- StandardDeviation(tmp.var, weights = weights)
                f.vals <- StatisticsByGroup(tmp.var/(2*tmp.sd), group = group, weights = weights)
                f.cols <- format.percentage.fill.colors
            }
            cell.fill[i,which(f.vals >  0.1)] <- f.cols[3]
            cell.fill[i,which(f.vals >  0.2)] <- f.cols[4]
            cell.fill[i,which(f.vals < -0.1)] <- f.cols[2]
            cell.fill[i,which(f.vals < -0.2)] <- f.cols[1]
        }
    }

    if (!show.question.name)
        row.labels <- FALSE
    output <- CreateCustomTable(result.formatted, row.header.labels = row.labels, 
                      row.spans = row.span, cell.fill = cell.fill, 
                      cell.font.color = results.font.color,
                      row.span.fill = row.span.fill, row.header.fill = row.header.fill, 
                      corner.fill = corner.fill, corner = corner.text,
                      col.header.fill = col.header.fill,
                      font.unit = font.unit, font.size = font.size, col.widths = col.widths,
                      row.span.pad = row.span.pad, row.header.pad = row.header.pad, 
                      row.header.font.weight = row.header.font.weight,
                      row.span.font.weight = row.span.font.weight,
                      col.header.font.weight = col.header.font.weight, 
                      suppress.nan = FALSE, suppress.na = FALSE,
                      num.header.rows = 2, row.height = row.height, ...)
    result.rows <- unlist(sapply(row.span, function(r) rep(r$label, r$height)))
    rownames(result) <- paste0(result.rows, ": ", row.labels)
    attr(output, "ChartData") <- result
    # Store p-values for testing
    if (font.color.set.if.nonsignificant)
        attr(output, "p-values") <- pvals
    return(output)
}
