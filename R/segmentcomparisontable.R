
#' @importFrom flipFormat CreateCustomTable
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom flipStatistics WeightedTable Table StatisticsByGroup
#' @importFrom stats sd
SegmentComparisonTable <- function(x, group, weights = NULL, subset = TRUE,
                                   format.numeric.decimals = 1,
                                   format.percentage.decimals = 0,
                                   format.conditional.fill = TRUE,
                                   format.numeric.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",
                                   format.percentage.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",
                                   show.index.values = FALSE,
                                   ...) # extra parameters to pass to CreateCustomTable
{

    format.percentage.fill.colors <- ConvertCommaSeparatedStringToVector(format.percentage.fill.colors)

    counts <-t(WeightedTable(group))
    result <- rbind(counts, counts/sum(counts))
    row.labels <- c(" ", " ")
    row.span <- list(list(label = " ", height = 2))
    row.format <- c("numeric", "percentage")
    row.vvi <- c(0, 0)
    row.vcol <- c(0, 0)

    v.list <- ProcessQVariables(x)
    for (vvi in 1:length(v.list))
    {
        vv <- v.list[[vvi]]
        if (isTRUE(attr(vv, "questiontype") %in% c("NumberMulti")))
        {
            tmp <- t(StatisticsByGroup(vv, group = group))
        } else
            tmp <- crosstabOneVariable(vv, group = group)

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
            tmp <- sweep(tmp, 2, colSums(tmp), "/")

        result <- rbind(result, tmp)
    }

    rownames(result) <- NULL # result is preserved in numeric form for exporting
    result.formatted <- matrix("", nrow(result), ncol(result))
    max.width <- max(3, ceiling(log10(max(result))), na.rm = TRUE) +
                 max(format.numeric.decimals, format.percentage.decimals) + 1
    patt.numeric <- paste0("%", max.width, ".", format.numeric.decimals, "f ")
    patt.percentage <- paste0("%", max.width, ".", format.percentage.decimals, "f%%")
    for (i in 1:nrow(result))
        result.formatted[i,] <- if (row.format[i] == "numeric") formatC(result[i,], format.numeric.decimals, format = "f")
                                else                            paste0(formatC(result[i,] * 100, format.percentage.decimals, format = "f"), "%")
    result.formatted[!is.finite(result)] <- ""
    result.formatted <- formatC(result.formatted, format = "s", width = max(nchar(result.formatted)))
    result.formatted <- gsub(" ", "&nbsp;", result.formatted)
    colnames(result.formatted) <- colnames(result)

    cell.fill <- "#FFFFFF"
    if (format.conditional.fill)
    {
        cell.fill <- matrix(cell.fill, nrow(result), ncol(result))
        for (i in 2:nrow(result))
        {
            if (row.format[i] == "percentage") # and if show index values is used
            {
                cell.fill[i,which(result[i,] <  0.2)] <- format.percentage.fill.colors[4]
                cell.fill[i,which(result[i,] <  0.1)] <- format.percentage.fill.colors[3]
                cell.fill[i,which(result[i,] < -0.1)] <- format.percentage.fill.colors[2]
                cell.fill[i,which(result[i,] < -0.2)] <- format.percentage.fill.colors[1]
            } else
            {
                # What about filters and weights?
                tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                           else                  v.list[[row.vvi[i]]][,row.vcol[i]]
                tmp.mean <- mean(tmp.var)
                tmp.sd <- sd(tmp.var)
                tmp.se <- ComputeSE(tmp.var)
                cat("sd:", tmp.sd, "se:", tmp.se, "\n")

                tmp.gmean <- StatisticsByGroup((tmp.var - tmp.mean)/(2*tmp.sd), group = group)

                cell.fill[i,which(tmp.gmean <  0.2)] <- format.percentage.fill.colors[4]
                cell.fill[i,which(tmp.gmean <  0.1)] <- format.percentage.fill.colors[3]
                cell.fill[i,which(tmp.gmean < -0.1)] <- format.percentage.fill.colors[2]
                cell.fill[i,which(tmp.gmean < -0.2)] <- format.percentage.fill.colors[1]

            }
        }
    }
    CreateCustomTable(result.formatted, row.header.labels = row.labels, row.spans = row.span,
                     row.span.fill = "#EFEFEF", row.header.fill = "#EFEFEF", corner.fill = "#EFEFEF",
                     col.header.fill = "#EFEFEF", cell.fill = cell.fill,
                     font.unit = "px", cell.font.size = 10, row.header.font.size = 10,
                     row.span.font.size = 10, col.header.font.size = 10)
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

ComputeSE <- function(x, w = NULL, is.binary = FALSE)
{
    # Filtering happens here
    n.observations <- length(x)
    if (is.null(w))
        w <- rep(1, n.observations)
    ww = w * w
    xw = x * w
    xxw = x * xw
    xww = xw * w
    xxww = xxw * w
    sum.x = sum(x)
    sum.w = sum(w)
    sum.xw = sum(xw)
    sum.ww = sum(ww)
    sum.xxw = sum(xxw)
    sum.xww = sum(xww)
    sum.xxww = sum(xxww)
    mean <- sum.xw/sum.w

    if (is.binary) # Numerical precision
    {
        if (mean < 0.00000001)
            mean = 0
        else if (mean > 0.99999999)
            mean = 1
    }

    bessel.correction = n.observations / (n.observations - 1)
    mean2 = mean * mean
    sum_of_squares = sum.xxw - 2 * mean * sum.xw + mean2 * sum.w
    sum_of_squares.w = sum.xxww - 2 * mean * sum.xww + mean2 * sum.ww

    taylor = sum_of_squares.w / (sum.w * sum.w) * bessel.correction
    return(sqrt(taylor))
}



