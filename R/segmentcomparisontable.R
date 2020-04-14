#' \code{SegmentComparisonTable}
#'
#' Creates a html table allowing comparison between segments using
#' variables or variable sets
#' @param x A list of variables or variable sets. These can be numeric or
#'    categorical (PickOne or PickAny). It is expected that the attributes
#'    for "question", "label" and "questiontype" is defined.
#' @param group The segmentation variable. Should be a factor.
#' @param row.names.to.remove Character vector or delimited string of
#'     row labels specifying rows to remove from the returned table.
#' @param weights Numeric; An optional vector of sampling weights.
#'     Should be the same length as \code{group}.
#' @param subset An optional vector specifying a subset of observations
#'      to be used.
#' @param format.numeric.decimals The number of decimals shown in the
#'      output table for numeric values.
#' @param format.percentage.decimals The number of decimals shown in
#'      the output table for percentage values.
#' @param format.conditional.fill Deprecated. Whether the fill color
#'      of the cells should reflect the value in the cells.
#' @param format.numeric.fill.colors Deprecated.
#' @param format.percentage.fill.colors Deprecated
#' @param cond.shade What should be shaded to reflect the magnitude of
#'      the value in each cell. One of "None", "Cell colors",
#'      "Font colors", "Boxes", "Arrows", Fonts and arrows".
#' @param cond.shade.colors A vector of 4 colors (in order) which
#'      will be used when values are 1) very small, 2)small, 3) large,
#'      4) very large.
#' @param cond.shade.cutoffs A vector of cutoffs used to evaluate
#'      whether values are very small/small/large/very large.
#'      Categorical variables are standardized by dividing by
#'      the same statistic computed over the whole populatoon.
#'      Numeric variables are standardized by dividing twice the
#'      standard deviation http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf
#' @param cond.shade.sig.only Logical; whether conditional shading
#'      should only be applied to cells with significant values.
#' @param cond.box.width Numeric; line width of box when
#'      \code{cond.shade == "Boxes"}.
#' @param cond.box.radius Numeric; roundness of box corner when
#'      \code{cond.shade == "Boxes"} (e.g. 0 for sharp corners,
#'      50 for oval).
#' @param cond.bar.radius Numeric; roundness of bar corner when
#'      \code{cond.shade == "Bars"} (e.g. 0 for sharp corners,
#'      50 for oval).
#' @param show.index.values Values are shown as a ratio to the total
#'     computed on the whole population (i.e. unsegmented).
#' @param cell.fill The default background color of the cells in the table.
#' @param summary.cell.fill The background color of the first two rows in
#'     the table which gives a summary of the segmentation variable.
#' @param summary.header.fill The background color of the row headers
#'     in the first two rows in the table which given a summary of the
#'     segmentation variable.
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
#' @param font.color.nonparametric Logical; whether a non-parametric i
#'      test should be used.
#' @param show.question.name Whether the question name should be shown in the output table.
#' @param col.header.labels A vector or comma-separated labels to
#'      override the column name
#' @param col.widths Vector or comma-separated list to control
#'      column widths.
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
                                   row.names.to.remove = "",
                                   format.numeric.decimals = 1,
                                   format.percentage.decimals = 0,
                                   format.conditional.fill = NULL,      # deprecated - use cond.shade instead
                                   format.numeric.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",
                                   format.percentage.fill.colors = "#E99598, #E5C8C4, #A9C0DA, #82A5CB",

                                   cond.shade = c("None", "Cell colors", "Font colors", "Boxes", "Arrows", "Fonts and arrows")[2],
                                   cond.box.radius = 0,
                                   cond.box.width = 2,
                                   cond.bar.radius = 13,
                                   cond.shade.colors = c("#E99598", "#E5C8C4", "#A9C0DA", "#82A5CB"), # a vector of 4
                                   cond.shade.cutoffs = c(-0.2, -0.1, 0.1, 0.2),
                                   cond.shade.sig.only = FALSE,
                                   show.index.values = FALSE,
                                   cell.fill = "#FFFFFF",
                                   font.color = "#2C2C2C",
                                   font.size = 10,
                                   font.unit = "px",
                                   row.height = paste0(font.size + 5, font.unit),
                                   font.color.set.if.nonsignificant = TRUE,
                                   font.color.nonsignificant = "#999999",
                                   font.color.confidence = 0.95,
                                   font.color.FDRcorrection = FALSE,
                                   font.color.nonparametric = FALSE,
                                   show.question.name = TRUE,
                                   col.header.labels = NULL,
                                   col.widths = "100px, 100px",
                                   row.header.font.weight = "normal",
                                   row.span.font.weight = "normal",
                                   col.header.font.weight = "normal",
                                   row.header.pad = 5,
                                   row.span.pad = row.header.pad,
                                   row.header.fill = "#F1F3F4",
                                   row.span.fill = row.header.fill,
                                   col.header.fill = "#AEB7BA",
                                   corner.fill = col.header.fill,
                                   summary.header.fill = col.header.fill,
                                   summary.cell.fill = "#D1D7D9",
                                   ...)
{
    if (!is.list(x) || length(x[[1]]) <= 1)
        stop("Input should be a list of variables or variable sets.")
    group.label <- attr(group, "label") # group should be a variable not a variable set
    if (is.null(group.label))
        group.label <- " "
    corner.text <- ""

    group <- ProcessQVariables(group)
    if (!is.factor(group))
        stop("'Segmentation' should be a nominal or ordinal variable.")
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

        # Use question attributes to names variables
        if (v.qtype == "Date")
            colnames(vv) <- levels(attr(x[[vvi]], "QDate"))
        else if (ncol(vv) == 1)
            colnames(vv) <- attr(x[[vvi]], "label")

        # Remove variables
        if (sum(nchar(row.names.to.remove), na.rm = TRUE) > 0)
        {
            rm.names <- ConvertCommaSeparatedStringToVector(row.names.to.remove, text.qualifier = "\"")
            rm.patt <- paste(c(paste0("^", rm.names, ", "), paste0(", ", rm.names, "$")), collapse = "|")
            net.ind <- if (v.qtype %in% c("NumberGrid", "PickOneMulti")) grep(rm.patt, colnames(vv), perl = TRUE)
                       else which(colnames(vv) %in% rm.names)
            if (length(net.ind) > 0)
                vv <- vv[, -net.ind, drop = FALSE]
        }

        # Compute main statistic (average/percentage)
        tmp <- t(StatisticsByGroup(vv, group = group, weights = weights))
        if (v.qtype %in% c("PickOne", "Date"))
            tmp <- sweep(tmp, 2, colSums(tmp), "/")

        if (NROW(tmp) == 1 && rownames(tmp)[1] == attr(x[[vvi]], "question"))
            rownames(tmp) <- ""
        tmp.nvar <- ncol(vv)
        row.vcol <- c(row.vcol, 1:tmp.nvar)
        row.qlabel <- attr(x[[vvi]], "question")
        if (is.null(row.qlabel))
            row.qlabel <- attr(x[[vvi]], "label")
        if (is.null(row.qlabel))
            row.qlabel <- names(x)[vvi]
        if (is.null(row.qlabel))
            row.qlabel <- ""
        row.span[[length(row.span) + 1]] <- list(label = row.qlabel, height = tmp.nvar)
        if (!is.null(rownames(tmp)))
            row.labels <- c(row.labels, rownames(tmp))
        tmp.numeric <- isTRUE(attr(x[[vvi]], "questiontype") %in% c("Number", "NumberMulti", "NumberGrid"))
        row.format <- c(row.format, rep(if (tmp.numeric) "numeric" else "percentage", tmp.nvar))
        row.vvi <- c(row.vvi, rep(vvi, tmp.nvar))
        v.list[[vvi]] <- vv # save subsetted variable

        # Always compute index values because they are used for cell fill
        ind.not.missing <- !is.na(group)
        tot.mean <- Mean(vv[ind.not.missing,,drop = FALSE], weights = weights[ind.not.missing])
        if (NROW(tmp) > 1)
            index.values[[vvi]] <- sweep(tmp, 1, tot.mean, "/")
        else
            index.values[[vvi]] <- tmp/tot.mean

        if (show.index.values)
            tmp <- index.values[[vvi]]
        result <- rbind(result, tmp)
    }
    rownames(result) <- NULL # result is preserved in numeric form for exporting
    result.formatted <- matrix("", nrow(result), ncol(result))
    result.formatted[1,] <- formatC(result[1,], digits = 0, format = "f", big.mark = ",") # first row is always unweighted sample size
    for (i in 2:nrow(result))
        result.formatted[i,] <- if (!show.index.values && row.format[i] == "numeric") formatC(result[i,], format.numeric.decimals, format = "f", big.mark = ",")
                                else paste0(formatC(result[i,] * 100, format.percentage.decimals, format = "f", big.mark = ","), "%")
    result.formatted[!is.finite(result)] <- ""
    result.formatted <- formatC(result.formatted, format = "s", width = max(nchar(result.formatted)))
    if (cond.shade != "Bars")
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
        {
            old.dim <- dimnames(pvals)
            pvals <- PValueAdjustFDR(pvals, alpha = 1 - font.color.confidence)
            pvals <- matrix(pvals, nrow(result), ncol(result), dimnames = old.dim)
        }
        results.font.color[which(pvals < 1 - font.color.confidence)] <- font.color
    }

    # Conditionally color based on cell value
    cell.align <- "center"
    cell.pad <- 0
    cell.fill <- matrix(cell.fill, nrow(result), ncol(result))
    cell.fill[1:2,] <- summary.cell.fill
    prefix <- matrix("", nrow(result), ncol(result))
    suffix <- matrix("", nrow(result), ncol(result))

    if ((cond.shade != "None" && is.null(format.conditional.fill)) ||
        isTRUE(format.conditional.fill))
    {
        if (cond.shade == "Bars")
        {
            cell.align <- "right"
            cell.pad <- matrix(5, nrow(result), ncol(result))
        }

        cond.levels <- length(cond.shade.cutoffs)
        cond.midlevel <- floor(cond.levels/2)
        for (i in 3:nrow(result))
        {
            if (row.format[i] == "percentage")
            {
                f.vals <- as.numeric((index.values[[row.vvi[i]]])[row.vcol[i],] - 1.0)
                f.cols <- cond.shade.colors
            } else
            {
                tmp.var <- if (row.vcol[i] == 0) v.list[[row.vvi[i]]]
                           else                  v.list[[row.vvi[i]]][,row.vcol[i]]
                tmp.sd <- StandardDeviation(tmp.var, weights = weights)
                tmp.mean <- Mean(tmp.var, weights = weights)
                f.vals <- StatisticsByGroup((tmp.var-tmp.mean)/(2*tmp.sd), group = group, weights = weights)
                f.cols <- cond.shade.colors
            }

            # Set up indexes allocating shading color
            c.ind <- list()
            c.ind[[1]] <- which(f.vals < cond.shade.cutoffs[1])
            for (j in 2:(cond.midlevel-1))
            {
                k <- cond.levels - j + 1
                if (k <= 1 || k >= cond.levels)
                    break
                c.ind[[j]] <- which(f.vals >= cond.shade.cutoffs[j-1] & f.vals < cond.shade.cutoffs[j])
                c.ind[[k]] <- which(f.vals <= cond.shade.cutoffs[k+1] & f.vals > cond.shade.cutoffs[k])
            }
            c.ind[[cond.levels]] <- which(f.vals > cond.shade.cutoffs[cond.levels])
            if (cond.shade.sig.only && font.color.set.if.nonsignificant)
            {
                for (j in 1:4)
                    c.ind[[j]] <- intersect(c.ind[[j]],
                        which(pvals[i,] < 1 - font.color.confidence))
            }


            if (cond.shade == "Cell colors" || isTRUE(format.conditional.fill))
            {
                for (j in 1:cond.levels)
                    cell.fill[i,c.ind[[j]]] <- f.cols[j]
            }
            else
            {
                for (j in 1:cond.levels)
                {
                    arrow.sym <- if (j <= cond.midlevel) "&#x2193;" else "&#x2191"
                    if (cond.shade == "Boxes")
                        tmp.prefix <- paste0("<span style='border:", cond.box.width,
                        "px solid ", f.cols[j], "; border-radius:", cond.box.radius,
                        "%; padding-right: 5px;'>")
                    else if (cond.shade == "Bars")
                    {
                        tmp.prefix <- ""
                        if (any(is.finite(f.vals)) && any(f.vals > 0))
                        {
                            max.val <- max(abs(as.numeric(f.vals)), na.rm = TRUE)
                            tmp.vals <- round(abs(f.vals[c.ind[[j]]])/max.val * 100)
                            tmp.prefix <- paste0("<div style='padding: 0px 0px;'><div style='background:", f.cols[j], "; width: calc(", tmp.vals, "% - 5px); height: 100%; float:right; vertical-align:middle; overflow: visible; white-space: nowrap; padding-right: 5px; border: solid 1px ", f.cols[j], "; border-radius: ", cond.bar.radius, "%; direction: rtl;'>&lrm;")
                            cell.pad[i,c.ind[[j]]] <- 0
                        }
                    }
                    else if (cond.shade == "Arrows")
                        tmp.prefix <- ""
                    else
                        tmp.prefix <- paste0("<span style='color:", f.cols[j], "'>")
                    prefix[i,c.ind[[j]]] <- tmp.prefix

                    if (cond.shade %in% c("Font colors", "Boxes"))
                        suffix[i,c.ind[[j]]] <- "</span>"
                    else if (cond.shade == "Fonts and arrows")
                        suffix[i,c.ind[[j]]] <- paste0(arrow.sym, "</span>")
                    else if (cond.shade == "Arrows")
                        suffix[i,c.ind[[j]]] <- paste0("<span style='color:",
                        f.cols[j], "'>", arrow.sym, "</span>")
                    else if (cond.shade == "Bars")
                        suffix[i,c.ind[[j]]] <- "</div></div>"

                   # Realign cells after adding arrow
                   if (cond.shade %in% c("Arrows", "Fonts and arrows"))
                       result.formatted[i,c.ind[[j]]] <- sub("&nbsp;", "&nbsp;&nbsp;",
                       result.formatted[i,c.ind[[j]]])
                }
            }
        }
    }
    row.span.fill <- c(summary.header.fill, rep(row.span.fill, nrow(result)-1))
    row.header.fill <- c(rep(summary.header.fill, 2),
                         rep(row.header.fill, nrow(result) -2))

    if (!show.question.name)
        row.labels <- FALSE
    output <- CreateCustomTable(result.formatted, row.header.labels = row.labels,
                      row.spans = row.span, cell.fill = cell.fill,
                      cell.font.color = results.font.color,
                      row.span.fill = row.span.fill, row.header.fill = row.header.fill,
                      corner.fill = corner.fill, corner = corner.text,
                      col.header.fill = col.header.fill,
                      col.header.labels = col.header.labels,
                      font.unit = font.unit, font.size = font.size, col.widths = col.widths,
                      row.span.pad = row.span.pad, row.header.pad = row.header.pad,
                      row.header.font.weight = row.header.font.weight,
                      row.span.font.weight = row.span.font.weight,
                      col.header.font.weight = col.header.font.weight,
                      suppress.nan = FALSE, suppress.na = FALSE,
                      num.header.rows = 2, row.height = row.height,
                      cell.prefix = prefix, cell.suffix = suffix,
                      cell.align.horizontal = cell.align, cell.pad = cell.pad,
                      global.font.color = font.color, ...)
    result.rows <- trimws(unlist(sapply(row.span, function(r) rep(r$label, r$height))))
    tmp.minchar <- pmin(nchar(result.rows), nchar(row.labels))
    tmp.sep <- ifelse(tmp.minchar > 0, ": ", "")
    tmp.rownames <- paste0(result.rows, tmp.sep, row.labels)
    rownames(result) <- tmp.rownames
    attr(output, "ChartData") <- result

    # Store p-values for testing
    if (font.color.set.if.nonsignificant)
        attr(output, "p-values") <- pvals
    # Some extra info for KMeans
    attr(output, "question.labels") <- row.span
    if (font.color.set.if.nonsignificant)
    {
        attr(output, "confidence") <- font.color.confidence
        attr(output, "FDR correction") <- font.color.FDRcorrection
        attr(output, "nonparametric") <- font.color.nonparametric
    }
    return(output)
}
