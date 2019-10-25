
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
                                   font.color = "#2C2C2C", font.size = 10,
                                   font.color.set.if.nonsignificant = TRUE,
                                   font.color.nonsignificant = "#CCCCCC", font.color.confidence = 0.95,
                                   font.color.FDRcorrection = FALSE,
                                   ...) # extra parameters to pass to CreateCustomTable
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
    row.labels <- c(" ", " ")
    row.span <- list(list(label = " ", height = 2))
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
            cat(row.labels[i], ":", "i =", i, "vvi =", row.vvi[i], "vcol =", row.vcol[i], "\n")
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
    if (format.conditional.fill)
    {
        cell.fill <- matrix(cell.fill, nrow(result), ncol(result))
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
                # What about filters and weights?
                tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                           else                  v.list[[row.vvi[i]]][,row.vcol[i]]
                tmp.mean <- Mean(tmp.var, weights = weights)
                tmp.sd <- StandardDeviation(tmp.var, weights = weights)
                tmp.gmean <- StatisticsByGroup((tmp.var - tmp.mean)/(2*tmp.sd), group = group, weights = weights)
                cat("Standardizing numeric variables\n")
                print(tmp.gmean)

                # These values seem strance 
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
                     font.unit = "px", font.size = font.size, cell.font.color = results.font.color)
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

PValsByGroup <- function(x, group, weights, is.binary = FALSE)
{
    if (!is.factor(group))
        group <- factor(group)
    n.levels <- nlevels(group)
    levs <- levels(group)

    pval <- rep(NA, n.levels)
    names(pval) <- levels(pval)
    if (n.levels < 2)
        return(pval)

    for (i in 1:n.levels)
    {
        if (!is.binary)
        {
            ind.in <- which(group == levs[i])
            ind.out <- which(group != levs[i]) 

            stats.in <- ComputeStats(x[ind.in], weights[ind.in])
            stats.out <- ComputeStats(x[ind.out], weights[ind.out])
            deg.freedom <- (stats.in$sesq + stats.in$sesq)^2 /
                           ((stats.in$sesq^2)/(stats.in$n - 1) + (stats.out$sesq^2)/(stats.out$n - 1))
            t.statistic <- (stats.in$mean - stats.out$mean)/sqrt(stats.in$sesq + stats.out$sesq)
            pval[i] <- 2 * pt(abs(t.statistic), deg.freedom, lower.tail = FALSE)
            cat(levs[i], ": mean =", stats.in$mean, "se =", sqrt(stats.in$sesq), "n =", stats.in$n, "p =", pval[i], "\n")
        } else
        { 
            tmp <- CellStatistic(x, y = group == levs[i])
            pval[i] <- tmp["p"]
            cat(levs[i], ": se =", tmp["Standard Error"], "\n")
        }
    }
    return(pval)
}
 

# returns mean, std.err and n for computing p-values
ComputeStats <- function(x, w = NULL, is.binary = FALSE)
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
    #return(sqrt(taylor)) # standard error but we mainly use se^2
    return (list(mean = mean, sesq = taylor, n = n.observations)) # n is used for degrees of freedom - not weighted?

}
