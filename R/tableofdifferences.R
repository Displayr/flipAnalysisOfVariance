#' \code{Table of differences}
#'
#' Creates a html table allowing comparison between values in
#'      two tables with the same row/column labels.
#' @param table1 A Q Table containing a primary statistic, "Standard Error",
#'      and "n" or "Count". The output table will compare \code{table1}
#'      and \code{table2}, with values to highlight cells which are
#'      significantly different.
#' @param table2 A Q Table with the same labels as \code{table1}.
#' @param show Controls text shown in the output table. Select one of
#'      "Primary statistic of Table 2 with differences",
#'      "Primary statistic of Table 2" or "Differences".
#' @param cond.shade Controls which elements are colored differently
#'      to show significance. Select one of "None", "Cell colors", "Arrows", "Boxes".
#' @param cond.shade.cutoffs A vector of significance levels; cells will be
#'      colored if the p-value is smaller than one of the cutoffs.
#' @param cond.shade.ub.colors A vector of colors the same length as
#'      \code{cond.shade.cutoffs}. Cells will be colored if \code{table2}
#'      is significantly less than \code{table1}.
#' @param cond.shade.lb.colors A vector of colors the same length as
#'      \code{cond.shade.cutoffs}. Cells will be colored if \code{table2}
#'      is significantly greater than \code{table1}.
#' @param cond.shade.ub.bordercolors A vector of colors the same length as
#'      \code{cond.shade.cutoffs}. Cells will be colored if \code{table2}
#'      is significantly less than \code{table1}. These will be used
#'      to color the borders when \code{cond.shade == "Boxes"}.
#' @param cond.shade.lb.bordercolors A vector of colors the same length as
#'      \code{cond.shade.cutoffs}. Cells will be colored if \code{table2}
#'      is significantly greater than \code{table1} These will be used
#'      to color the borders when \code{cond.shade == "Boxes"}..
#' @param cell.fill The default background color of cells in the table.
#' @param font.family The font family of cells in the table.
#' @param font.color The font color of cells in the table.
#' @param cond.box.radius Numeric; roundness of box corner when
#'      \code{cond.shade == "Boxes"} (e.g. 0 for sharp corners,
#'      50 for oval).
#' @param cond.box.borderwidth Numeric; line width of box when
#'      \code{cond.shade == "Boxes"}.
#' @param cond.box.padding.right Number; controls how far the box extends
#'      from the text when \code{cond.shade == "Boxes"}.
#' @param cond.box.padding.left Number; controls how far the box extends
#'      from the text when \code{cond.shade == "Boxes"}.
#' @param cond.box.padding.top Number; controls how far the box extends
#'      from the text when \code{cond.shade == "Boxes"}.
#' @param cond.box.padding.bottom Number; controls how far the box extends
#'      from the text when \code{cond.shade == "Boxes"}.
#' @param cond.arrow.size Size of the arrows in font units.
#' @param font.family Default font family of the cells in the table.
#' @param font.color Default font color of the cells in the table.
#' @param font.size Default font size.
#' @param font.unit One of "px" or "pt".
#' @param format.statistic.decimals Numeric; The number of decimals shown
#'      if the primary statistic is shown in the cells.
#' @param format.statistic.prefix Optional text that will be prepended
#'      to the primary statistic if it is shown in the cells.
#' @param format.statistic.suffix Optional text that will be appended
#'      to the primary statistic if it is shown in the cells.
#' @param format.statistic.font.family Font family of primary statistic.
#' @param format.statistic.font.size Font size of primary statistic.
#' @param format.statistic.font.autocolor Whether the font of the
#'      primary statistic should be automatically set to black or white
#'      to maximise the contrast to the cell fill.
#'      Overrides \code{format.statistic.font.color}.
#' @param format.statistic.font.color Font color of primary statistic if it
#'      is shown in the cell.
#' @param format.difference.sign Logical; whether a +/- sign should
#'      always be prepended to the difference.
#' @param format.difference.decimals Numeric; The number of decimals shown
#'      if the difference is shown in the cells.
#' @param format.difference.prefix Optional text that will be prepended
#'      to the difference if it is shown in the cells.
#' @param format.difference.suffix Optional text that will be appended
#'      to the difference if it is shown in the cells.
#' @param format.difference.font.family Font family of difference.
#' @param format.difference.font.size Font size of difference.
#' @param format.difference.font.autocolor Whether the font of the
#'      difference should be automatically set to black or white
#'      to maximise the contrast to the cell fill.
#'      Overrides \code{format.difference.font.color}.
#' @param format.difference.font.color Font color of difference if it
#'      is shown in the cell.
#' @param legend.show Logical; whether or not to show legend at the
#'      bottom of the table.
#' @param legend.sep Character; the string used to separate entries in
#'      the legend.
#' @param legend.fill Background color of the region behind the legend.
#' @param legend.lineheight Numeric; controls the spacing between
#'      lines in the legend. It is applied as a multiple of the font size.
#' @param legend.decimals Numeric; the number of decimal places to
#'      show for confidence levels in the legend.
#' @param legend.font.family Font family of text in the legend.
#' @param legend.font.color Font color of text in the legend.
#' @param legend.font.size Font size of text in the legend.
#' @param row.names.to.remove Character vector or delimited string of
#'     row labels specifying rows to remove from the returned table.
#' @param column.names.to.remove Character vector or delimited string of
#'     column labels specifying columns to remove from the returned table.
#' @param ... Other parameters passed to \link[flipFormat]{CreateCustomTable}.
#' @importFrom flipFormat CreateCustomTable
#' @importFrom flipTables RemoveRowsAndOrColumns
#' @export

TableOfDifferences <- function(table1,
                               table2,
                               show = c("Primary statistic of Table 2 with differences"),
                               cond.shade = c("None", "Cell colors", "Arrows", "Boxes")[2],
                               cond.shade.cutoffs = c(0.05, 0.1),
                               cond.shade.ub.colors = c("#82A5CB", "#A9C0DA"),
                               cond.shade.lb.colors = c("#E99598", "#E5C8C4"),
                               cond.shade.ub.bordercolors = cond.shade.ub.colors,
                               cond.shade.lb.bordercolors = cond.shade.lb.colors,
                               cell.fill = "#FFFFFF",
                               font.color = "#2C2C2C",
                               font.size = 10,
                               font.unit = "px",
                               font.family = "Arial",
                               legend.show = TRUE,
                               legend.sep = " ",
                               legend.fill = "transparent",
                               legend.font.family = font.family,
                               legend.font.color = font.color,
                               legend.font.size = font.size,
                               legend.lineheight = 2,
                               legend.decimals = 1,
                               format.statistic.decimals = NULL,
                               format.statistic.prefix = "",
                               format.statistic.suffix = "",
                               format.statistic.font.autocolor = TRUE,
                               format.statistic.font.color = "#2C2C2C",
                               format.statistic.font.size = font.size,
                               format.statistic.font.family = font.family,
                               format.difference.decimals = NULL,
                               format.difference.prefix = "",
                               format.difference.suffix = "",
                               format.difference.sign = TRUE,
                               format.difference.font.autocolor = TRUE,
                               format.difference.font.color = font.color,
                               format.difference.font.size = font.size,
                               format.difference.font.family = font.family,
                               cond.arrow.size = font.size,
                               cond.box.radius = 0,
                               cond.box.borderwidth = 2,
                               cond.box.padding.right = 5,
                               cond.box.padding.left = 0,
                               cond.box.padding.top = 0,
                               cond.box.padding.bottom = 0,
                               row.names.to.remove = "NET, Total, Sum",
                               column.names.to.remove = "NET, Total, Sum",
                               ...)
{
    # Check input data
    q.type <- attr(table1, "questiontype")
    if (!is.null(attr(table1, "statistic")) || !is.null(attr(table2, "statistic")))
        stop("Input tables must contain cell statistics for the sample size and standard error.")

    table1 <- convertToTableWithStatistics(table1)
    table2 <- convertToTableWithStatistics(table2)
    table1 <- RemoveRowsAndOrColumns(table1, row.names.to.remove, column.names.to.remove)
    table2 <- RemoveRowsAndOrColumns(table2, row.names.to.remove, column.names.to.remove)
    if (any(rownames(table1) != rownames(table2)))
        stop("Row names of Table 1 and Table 2 should be identical and in the same order.")
    if (any(colnames(table1) != colnames(table2)))
        stop("Column names of Table 1 and Table 2 should be identical and in the same order.")


    # Check that input tables contain the required statistics
    # which depends on the primary statistic in the table
    stat1 <- dimnames(table1)[[3]]
    stat2 <- dimnames(table2)[[3]]
    if (stat1[1] != stat2[1])
        stop("The primary statistic of Table 1 (", stat1[1],
        ") does not match the primary statistic of Table 2 (",
        stat2[1], ").")

    allowed.primary <- c("Average", "%", "Total %", "Column %")
    if (isTRUE(q.type == "PickOneMulti"))
        allowed.primary <- c(allowed.primary, "Row %")
    if (!(stat1[1] %in% allowed.primary))
        stop("The primary statistic in the input table cannot be '", stat1[1],
             "'. A t-test can only be computed on '",
             paste(allowed.primary, collapse = "', '"), "'.")

    if (stat1[1] == "Column %")
    {
        n.stat.name <- c("Column Sample Size", "Column n")
        se.stat.name <- "Column Standard Error"

    } else if (stat1[1] == "Row %") # Only for PickOneMulti tables
    {
        n.stat.name <- c("Sample Size", "Base n")
        se.stat.name <- "Standard Error"

    } else
    {
        n.stat.name <- c("Sample Size", "Base n")
        se.stat.name <- "Standard Error"
    }

    # Get sample size and standard errors
    ind1.n <- findIndexOfStat(stat1, n.stat.name)
    ind2.n <- findIndexOfStat(stat2, n.stat.name)
    ind1.se <- findIndexOfStat(stat1, se.stat.name)
    ind2.se <- findIndexOfStat(stat2, se.stat.name)
    if (length(ind1.n) == 0 || length(ind1.se) == 0 ||
        length(ind2.n) == 0 || length(ind2.se) == 0)
        stop("To test whether the difference in the primary statistic '",
            stat1[1], "' is significant, input tables need to contain the cell statistic '",
            se.stat.name, "' and one of '", n.stat.name[1], "' or '", n.stat.name[2], "'.")

    # Compute significance of differences
    is.percentage <- grepl("%", stat1[1], fixed = TRUE)
    denom <- if (is.percentage) 100 else 1
    cell.diff <- table2[,,1] - table1[,,1]
    pvals <- independentSamplesTTestMeans(table2[,,1]/denom, table1[,,1]/denom,
                 table2[,,ind2.se], table1[,,ind1.se],
                 table2[,,ind2.n], table1[,,ind1.n], two.sided = FALSE)

    if (is.null(format.statistic.decimals))
        format.statistic.decimals <- if (is.percentage) 0 else 2
    if (is.null(format.difference.decimals))
        format.difference.decimals <- if (is.percentage) 0 else 2

    # Conditional shading
    cond.ord <- order(cond.shade.cutoffs, decreasing = TRUE)
    tmp.prefix <- matrix("", nrow = nrow(table1), ncol(table1))
    tmp.suffix <- matrix("", nrow = nrow(table1), ncol(table1))
    tmp.fill <- NULL
    if (cond.shade == "Cell colors")
    {
        cell.fill <- matrix(cell.fill, nrow = nrow(table1), ncol = ncol(table1))
        for (i in cond.ord)
        {
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff < 0)
            cell.fill[ind] <- cond.shade.lb.colors[i]
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff > 0)
            cell.fill[ind] <- cond.shade.ub.colors[i]
        }
    } else if (cond.shade == "Boxes")
    {
        cell.fill <- matrix(cell.fill, nrow = nrow(table1), ncol = ncol(table1))
        tmp.fill <- cell.fill
        for (i in cond.ord)
        {
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff < 0)
            tmp.prefix[ind] <- paste0("<span style='border:",
                cond.box.borderwidth, "px solid ",cond.shade.lb.bordercolors[i],
                "; background-color:", cond.shade.lb.colors[i],
                "; border-radius:", cond.box.radius, "%",
                "; padding-top:", cond.box.padding.top, "px",
                "; padding-bottom:", cond.box.padding.bottom, "px",
                "; padding-left:", cond.box.padding.left, "px",
                "; padding-right:", cond.box.padding.right, "px;'>")
            tmp.suffix[ind] <- "</span>"
            tmp.fill[ind] <- cond.shade.lb.colors[i]

            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff > 0)
            tmp.prefix[ind] <- paste0("<span style='border:",
                cond.box.borderwidth, "px solid ",cond.shade.ub.bordercolors[i],
                "; background-color:", cond.shade.ub.colors[i],
                "; border-radius:", cond.box.radius, "%",
                "; padding-top:", cond.box.padding.top, "px",
                "; padding-bottom:", cond.box.padding.bottom, "px",
                "; padding-left:", cond.box.padding.left, "px",
                "; padding-right:", cond.box.padding.right, "px;'>")
            tmp.suffix[ind] <- "</span>"
            tmp.fill[ind] <- cond.shade.ub.colors[i]
        }
    }

    # Construct text to be shown inside table cells
    if (show == "Primary statistic of Table 2" ||
        show == "Primary statistic of Table 2 with differences")
    {
        cell.text <- paste0(format.statistic.prefix,
            formatC(table2[,,1], format.statistic.decimals,
            format = "f", big.mark = ","),
            format.statistic.suffix)

    } else
    {
        cell.text <- paste0(format.difference.prefix,
            formatC(cell.diff, format.difference.decimals,
            format = "f", big.mark = ",",
            flag = if (format.difference.sign) "+" else ""),
            format.difference.suffix)
    }
    max.width <- max(nchar(cell.text))
    cell.text <- formatC(cell.text, format = "s", width = max.width)
    cell.text <- gsub(" ", "&nbsp;", cell.text)

    # This occurs later than the "Cell colors" and "Boxes" options
    # because we need to add spaces to cell.text to re-align characters
    if (cond.shade == "Arrows")
    {
        for (i in cond.ord)
        {
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff < 0)
            tmp.suffix[ind] <- paste0("<span style='color:",
                cond.shade.lb.colors[i], "; font-size:",
                cond.arrow.size, font.unit, "'>&#9660;</span>")
            cell.text[ind] <- sub("&nbsp;", "&nbsp;&nbsp;", cell.text[ind])
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff > 0)
            tmp.suffix[ind] <- paste0("<span style='color:",
                cond.shade.ub.colors[i], "; font-size:",
                cond.arrow.size, font.unit, "'>&#9650;</span>")
            cell.text[ind] <- sub("&nbsp;", "&nbsp;&nbsp;", cell.text[ind])
        }
    }

    cell.text <- paste0(tmp.prefix, cell.text, tmp.suffix)
    if (show == "Primary statistic of Table 2 with differences")
    {
        if (format.difference.font.autocolor)
            format.difference.font.color <- autoFontColor(cell.fill)
        tmp.text <- paste0(format.difference.prefix,
                     formatC(cell.diff, format.difference.decimals,
                     format = "f", big.mark = ",",
                     flag = if (format.difference.sign) "+" else ""),
                     format.difference.suffix)
        max.width <- max(nchar(tmp.text))
        tmp.text <- formatC(tmp.text, format = "s", width = max.width)
        tmp.text <- gsub(" ", "&nbsp;", tmp.text)
        cell.text <- paste0(cell.text,
                     "<span style='font-family:", format.difference.font.family,
                     "; color:", format.difference.font.color,
                     "; font-size:", format.difference.font.size, font.unit, "'>",
                     tmp.text,
                     "</span>")
    }

    # Set fonts to pass to CreateCustomTable
    # This needs to be done after the conditional shading
    # so the autoFontColor can be called appropriately
    if (show == "Differences")
    {
        cell.font.family = format.difference.font.family
        cell.font.size = format.difference.font.size
        if (format.difference.font.autocolor)
            format.difference.font.color <- if (cond.shade == "Boxes") autoFontColor(tmp.fill)
                                            else                       autoFontColor(cell.fill)
        cell.font.color = format.difference.font.color

    } else
    {
        cell.font.family = format.statistic.font.family
        cell.font.size = format.statistic.font.size
        if (format.statistic.font.autocolor)
            format.statistic.font.color <- if (cond.shade == "Boxes") autoFontColor(tmp.fill)
                                           else                       autoFontColor(cell.fill)
        cell.font.color = format.statistic.font.color
    }

    legend.text <- ""
    if (legend.show && cond.shade != "None")
    {
        conf <- (1 - cond.shade.cutoffs) * 100
        tmp.lev <- length(conf)
        tmp.fill <- c(cond.shade.ub.colors, rev(cond.shade.lb.colors))
        empty.text <- paste(rep("&nbsp;", 4), collapse = "")

        if (cond.shade == "Cell colors")
            tmp.text <- sprintf("<span style='border:1px solid %s; background-color: %s'>%s</span>",
                tmp.fill, tmp.fill, empty.text)
        else if (cond.shade == "Arrows")
            tmp.text <- sprintf("<span style='color:%s; font-size: %.0f%s'>%s</span>",
                tmp.fill, cond.arrow.size, font.unit, rep(c("&#9650;", "&#9660;"), each = tmp.lev))
        else if (cond.shade == "Boxes")
            tmp.text <- sprintf("<span style='border:%.0fpx solid %s; background-color: %s; border-radius:%.0f%%'>%s</span>",
                cond.box.borderwidth,
                c(cond.shade.ub.bordercolors, rev(cond.shade.lb.bordercolors)),
                tmp.fill, cond.box.radius, empty.text)

        tmp.text <- paste("<nobr>", tmp.text,
                        sprintf(paste0("Signficant %s at %.", legend.decimals, "f%% confidence level"),
                        rep(c("increase", "decrease"), each = tmp.lev),
                        c(conf, rev(conf))), "</nobr>")
        legend.text <- paste(tmp.text, collapse = legend.sep)
    }

    cell.text <- matrix(cell.text,
        nrow(table1), ncol(table1), dimnames = dimnames(table1)[1:2])
    result <- CreateCustomTable(cell.text, cell.fill = cell.fill,
        cell.font.family = cell.font.family, cell.font.size = cell.font.size,
        cell.font.color = cell.font.color,
        footer = legend.text, footer.fill = legend.fill,
        footer.font.color = legend.font.color,
        footer.font.family = legend.font.family,
        footer.font.size = legend.font.size,
        footer.lineheight = legend.lineheight,
        font.unit = font.unit, ...)
    attr(result, "p-values") <- pvals
    return(result)
}

#' Use black or white for good contrast against colors
#'
#' @param colors vector of colors which will be the background color of the
#' @importFrom grDevices col2rgb rgb2hsv
autoFontColor <- function (colors)
{
    tmp.rgb <- col2rgb(colors)
    tmp.lum <- apply(tmp.rgb, 2, function(x) return(0.299*x[1] + 0.587*x[2] + 0.114*x[3]))
    return(ifelse(tmp.lum > 126, "#2C2C2C", "#FFFFFF"))
}

# Tries to convert the QTable into a 3d array
# An error will be thrown if the input is not a QTable
# or it does not contain multiple statistics
convertToTableWithStatistics <- function(x)
{
    if (is.null(attr(x, "questions")) || is.null(attr(x, "name")))
        stop("Input tables must be Q Tables showing statistics computed from questions or variable sets")
    if (is.character(x))
        x <- array(suppressWarnings(as.numeric(x)), dim = dim(x), dimnames = dimnames(x))
    if (length(dim(x)) > 3)
        stop("Output cannot be produced because the input table has too many dimensions. ",
             "Try removing one of the questions from the crosstab or ",
             "splitting up the multinomial question.")

    # 1-column table with multiple stats in 2nd dimension
    if (length(dim(x)) == 2)
    {
        dn <- dimnames(x)
        dn <- c(dn[1], "", dn[2])
        x <- array(x, dim = sapply(dn, length), dimnames = dn)

    } else if (length(dim(x)) < 2)
        stop("Input tables must be Q Tables showing statistics computed from questions or variable sets")

    return(x)
}

# Tries to find target in the stat.names list
# If multiple entries in target are in stat.names, the index of the first match is returned
findIndexOfStat <- function(stat.names, target)
{
    ind <- integer(0)
    for (tt in target)
    {
        ind <- which(stat.names == tt)
        # stop after first match is found
        if (length(ind) > 0)
            break
    }
    return(ind)
}
