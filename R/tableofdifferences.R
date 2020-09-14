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
#' @param ... Other parameters passed to \link[flipFormat]{CreateCustomTable}.
#' @importFrom flipFormat CreateCustomTable
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
                               cond.box.radius = 0,
                               cond.box.borderwidth = 2,
                               cond.box.padding.right = 5,
                               cond.box.padding.left = 0,
                               cond.box.padding.top = 0,
                               cond.box.padding.bottom = 0,
                               ...)
{
    # 1-column table may be not have 3 dimensions
    stat1 <- dimnames(table1)[[3]]
    stat2 <- dimnames(table2)[[3]]

    if (stat1[1] != stat2[1])
        stop("The primary statistic of Table 1 (", stat1[1],
        ") does not match the primary statistic of Table 2 (",
        stat2[1], ").")
    # allow row/column names to be in different arrangements
    # retain only matching ones

    cell.diff <- primaryStat(table2) - primaryStat(table1)
    pvals <- independentSamplesTTestMeans(
        primaryStat(table1), primaryStat(table2),
        table1[,,"Standard Error"], table2[,,"Standard Error"],
        table1[,,"Count"], table2[,,"Count"])
    #print(pvals, digits = 5)

    is.percentage <- grepl("%", stat1[1], fixed = TRUE)
    if (is.null(format.statistic.decimals))
        format.statistic.decimals <- if (is.percentage) 0 else 2
    if (is.null(format.difference.decimals))
        format.difference.decimals <- if (is.percentage) 0 else 2

    if (show == "Primary statistic of Table 2" || 
        show == "Primary statistic of Table 2 with differences")
    {
        cell.text <- paste0(format.statistic.prefix, 
            formatC(primaryStat(table2), format.statistic.decimals, 
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

    # Conditional cell fill shading is specified before cell.text
    # so that autoFontColor can be computed
    # However conditional shading is arrows is performed later
    # so that text can re-aligned after adding extra character 
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
    } else if (cond.shade == "Arrows")
    {
        for (i in cond.ord)
        {
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff < 0)
            tmp.suffix[ind] <- paste0("<span style='color:",
                cond.shade.lb.colors[i], "'>&#9660;</span>")
            cell.text[ind] <- sub("&nbsp;", "&nbsp;&nbsp;", cell.text[ind])
            ind <- which(pvals < cond.shade.cutoffs[i] & cell.diff > 0)
            tmp.suffix[ind] <- paste0("<span style='color:",
                cond.shade.ub.colors[i], "'>&#9650;</span>")
            cell.text[ind] <- sub("&nbsp;", "&nbsp;&nbsp;", cell.text[ind])
        }
    } else if (cond.shade == "Boxes")
    {   
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

    cell.text <- matrix(cell.text,
        nrow(table1), ncol(table1), dimnames = dimnames(table1)[1:2])
    CreateCustomTable(cell.text, cell.fill = cell.fill,
        cell.font.family = cell.font.family, cell.font.size = cell.font.size,
        cell.font.color = cell.font.color)
}


primaryStat <- function(x)
{
    return(x[,,1])
}

#' use black or white for good contrast against colors
#'
#' @param colors vector of colors which will be the background color of the
#' @importFrom grDevices col2rgb rgb2hsv
autoFontColor <- function (colors)
{
    tmp.rgb <- col2rgb(colors)
    tmp.lum <- apply(tmp.rgb, 2, function(x) return(0.299*x[1] + 0.587*x[2] + 0.114*x[3]))
    return(ifelse(tmp.lum > 126, "#2C2C2C", "#FFFFFF"))
}
