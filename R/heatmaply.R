## TODO: roxygen documentation of all functions

# devtools::install_github("ropensci/plotly", ref = "fix/subplot")
# reference: https://plot.ly/ggplot2/ggdendro-dendrograms/
# to answer later: http://stackoverflow.com/questions/34733829/questions-about-a-tutorial-regarding-interactive-heatmaps-with-plotly
# to check: https://plot.ly/r/heatmaps/



#' @title Checks if an object is of class plotly or not.
#' @export
#' @description
#' Helpful for the plot_method in link{heatmaply}.
#'
#' @param x an object to check
#'
#' @return
#' TRUE if the object ingerits "plotly" as a class.
#'
is.plotly <- function(x) {
  inherits(x, "plotly")
}





#' @title  Cluster heatmap based on plotly
#' @name heatmaply
#'
#' @description
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#'
#' (Please submit an issue on github if you have a feature that you wish to have added)
#'
#' @param x can either be a heatmapr object, or a numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#'
#' @param colors,col a vector of colors to use for heatmap color.
#' The default uses 
#' \code{\link[viridis]{viridis}(n=256, alpha = 1, begin = 0, end = 1, option = "viridis")}
#' It is passed to \link[ggplot2]{scale_fill_gradientn}.
#' If colors is a color function (with the first argument being `n` = the number of colors),
#' it will be used to create 256 colors from that function.
#' (col is there to stay compatible with \link[gplots]{heatmap.2})
#' @param limits a two dimensional numeric vector specifying the data range for the scale.
#' @param na.value color to use for missing values (default is "grey50").
#'
#' @param row_text_angle numeric (Default is 0), the angle of the text of the 
#' rows. (this is called srtRow in \link[gplots]{heatmap.2})
#' @param column_text_angle numeric (Default is 45), the angle of the text of 
#' the columns. (this is called srtCol in \link[gplots]{heatmap.2})
#'
#' @param subplot_margin Currently not well implemented. It is passed to 
#' \link[plotly]{subplot}. Default is 0. Either a single value or
#'  four values (all between 0 and 1). If four values are provided,
#'  the first is used as the left margin, the second is used as the right margin,
#'  the third is used as the top margin, and the fourth is used as the bottom margin.
#'  If a single value is provided, it will be used as all four margins.
#'
#' @param cellnote Mouseover values for the data. Useful if applying scaling.
#' @param draw_cellnote Should the cellnote annotations be drawn? Defaults is FALSE,
#' if cellnote is not supplied, TRUE if cellnote is supplied. If TRUE and 
#' cellnote is not supplied, x will be used for cellnote.
#'
#' @param Rowv determines if and how the row dendrogram should be reordered.	
#' By default, it is TRUE, which implies dendrogram is computed and reordered 
#' based on row means. If NULL or FALSE, then no dendrogram is computed and 
#' no reordering is done. If a \link{dendrogram} (or \link{hclust}), 
#' then it is used "as-is", ie without any reordering. If a vector of integers, 
#' then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	
#' Has the options as the Rowv argument above and additionally when x is a 
#' square matrix, Colv = "Rowv" means that columns should be treated 
#' identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) 
#' between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering 
#' when Rowv or Colv are not dendrograms. Defaults to hclust.
#'
#' @param dist_method default is NULL (which results in "euclidean" to be used). 
#' Can accept alternative character strings indicating the
#' method to be passed to distfun. By default distfun. is \link{dist} hence
#' this can be one of "euclidean", "maximum", "manhattan", "canberra", "binary" 
#' or "minkowski".
#' @param hclust_method default is NULL (which results in "complete" to be used). 
#' Can accept alternative character strings indicating the
#' method to be passed to hclustfun By default hclustfun is \link{hclust} hence
#' this can be one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#'
#' @param distfun_row distfun for row dendrogram only.
#' @param hclustfun_row hclustfun for col dendrogram only.
#' @param distfun_col distfun for row dendrogram only.
#' @param hclustfun_col hclustfun for col dendrogram only.
#'
#' @param dendrogram character string indicating whether to draw 'none', 'row', 
#' 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) 
#' is FALSE or NULL and dendrogram is 'both', then a warning is issued and Rowv 
#' (or Colv) arguments are honoured.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the 
#' row and column dendrograms. The default uses stats{reorder.dendrogram}
#'
#' @param k_row an integer scalar with the desired number of groups by which to 
#' color the dendrogram's branches in the rows (uses \link[dendextend]{color_branches})
#' If NA then \link[dendextend]{find_k} is used to deduce the optimal number of clusters.
#' @param k_col an integer scalar with the desired number of groups by which to 
#' color the dendrogram's branches in the columns (uses \link[dendextend]{color_branches})
#' If NA then \link[dendextend]{find_k} is used to deduce the optimal number of clusters.
#'
#' @param symm logical indicating if x should be treated symmetrically; can only 
#' be true when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when missing) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#'
#' @param scale character indicating if the values should be centered and scaled 
#' in either the row direction or the column direction, or none. The default is 
#' "none".
#' @param na.rm logical indicating whether NA's should be removed.
#'
#' @param row_dend_left logical (default is FALSE). Should the row dendrogram be
#' plotted on the left side of the heatmap. If false then it will be plotted on 
#' the right side.
#'
#' @param margins numeric vector of length 4 (default is c(50,50,NA,0)) 
#' containing the margins (see \link[plotly]{layout}) for column, row and main 
#' title names, respectively. The top margin is NA by default. If main=="" 
#' then the top margin will be set to 0, otherwise it will get 30.
#' For a multiline title a larger default for the 3rd element should be set.
#' The right margin is NA by default, meaning it will be zero if row_dend_left 
#' is FALSE, or 100 if row_dend_left is TRUE.
#'
#' @param ... other parameters passed to \link{heatmapr} (currently, various 
#' parameters may be ignored.
#'
#' @param scale_fill_gradient_fun A function that creates a smooth gradient for the heatmap.
#' The default uses \link[ggplot2]{scale_fill_gradientn} with the values of colors, limits, and
#' na.value that are supplied by the user. The user can input a customized function, such as
#' \link{scale_color_gradient}() in order to get other results (although the virids default
#' is quite recommended)
#'
#' @param grid_color control the color of the heatmap grid. Default is NA. 
#' Value passed to \link[ggplot2]{geom_tile}. Do not use this parameter on 
#' larger matrix sizes, as it can dramatically prolong the build time of the heatmap.
#' (another parameter, grid_color, will be added in the future - once it is implemented in plotly)
#' In the meantime it is MUCH better to use the grid_gap argument.
#'
#' @param grid_gap this is a fast alternative to grid_color. The default is 0, but if a larger value
#' is used (for example, 1), then the resulting heatmap will have a white grid which can
#' help identify different cells. This is implemented using \link[plotly]{style} (with xgap and ygap).
#'
#' @param srtRow if supplied, this overrides row_text_angle (this is to stay compatible with \link[gplots]{heatmap.2})
#' @param srtCol if supplied, this overrides column_text_angle (this is to stay compatible with \link[gplots]{heatmap.2})
#'
#' @param xlab A character title for the x axis.
#' @param ylab A character title for the y axis.
#'
#' @param main A character title for the heatmap.
#'
#' @param titleX logical (TRUE). should x-axis titles be retained? (passed to \link[plotly]{subplot}).
#' @param titleY logical (TRUE). should y-axis titles be retained? (passed to \link[plotly]{subplot}).
#'
#'
#' @param hide_colorbar logical (FALSE). If TRUE, then the color bar is hidden.
#'
#' @param key.title (character) main title of the color key. If set to NULL (default) no title will be plotted.
#'
#' @param return_ppxpy logical (FALSE). If TRUE, then no plotting is done and the p, px and py objects are
#' returned (before turning into plotly objects). This is a temporary option which might be removed in the
#' future just to make it easy to create a ggplot heatmaps.
#'
#' @param row_side_colors,col_side_colors data.frame of factors to produce
#'    row/column side colors in the style of heatmap.2/heatmap.3.
#'    col_side_colors should be "wide", ie be the same dimensions
#'    as the column side colors it will produce.
#'
#' @param row_side_palette,col_side_palette Color palette functions to be
#'    used for row_side_colors and col_side_colors respectively.
#'
#' @param ColSideColors,RowSideColors passed to row_side_colors,col_side_colors in order
#' to keep compatibility with \link[gplots]{heatmap.2}
#'
#' @param plot_method Use "ggplot" or "plotly" to choose which library produces heatmap
#' and dendrogram plots
#' @param seriate character indicating the method of matrix sorting (default: "OLO").
#' Implemented options include:
#' "OLO" (Optimal leaf ordering, optimzes the Hamiltonian path length that is 
#' restricted by the dendrogram structure - works in O(n^4) )
#' "mean" (sorts the matrix based on the reorderfun using marginal means of 
#' the matrix. This is the default used by \link[gplots]{heatmap.2}),
#' "none" (the default order produced by the dendrogram),
#' "GW" (Gruvaeus and Wainer heuristic to optimze the Hamiltonian path length 
#' that is restricted by the dendrogram structure)
#'
#' @param heatmap_layers ggplot object (eg, theme_bw()) to be added to
#'  the heatmap before conversion to a plotly object.
#'
#' @param branches_lwd numeric (default is 0.6). The width of the dendrograms' branches.
#' If NULL then it is ignored. If the "lwd" is already defined in Rowv/Colv then this
#' parameter is ignored (it is checked using \link[dendextend]{has_edgePar}("lwd")).
#'
#'
#' @param file HTML file name to save the heatmaply into. Should be a character 
#' string ending with ".html".
#' For example: heatmaply(x, file = "heatmaply_plot.html").
#' This should not include a directory, only the name of the file.
#' You can relocate the file once it is created, or use \link{setwd} first.
#' This is based on \link[htmlwidgets]{saveWidget}.
#'
#' @param long_data Data in long format. Replaces x, so both should not be used.
#'  Colnames must be c("name", "variable", "value"). If you do not have a names
#'  column you can simply use a sequence of numbers from 1 to the number of "rows"
#'  inthe data.
#'
#' @param label_names Names for labells of x, y and value/fill mouseover.
#' @param fontsize_row,fontsize_col,cexRow,cexCol Font size for row and column labels.
#' @param subplot_widths,subplot_heights The relative widths and heights of each
#'  subplot. The length of these vectors will vary depending on the number of
#'  plots involved.
#'
#' @param colorbar_len The length of the colorbar/color key relative to the total
#' plot height. Only used if plot_method = "plotly"
#'
#' @param colorbar_xanchor,colorbar_yanchor The x and y anchoring points of the 
#' colorbar/color legend. Can be "left", "middle" or "right" for colorbar_xanchor,
#' and "top", "middle" or "bottom" for colorbar_yanchor.
#' See \code{\link[plotly]{colorbar}} for more details.
#' @param colorbar_xpos,colorbar_ypos The x and y co-ordinates (in proportion of the plot window)
#' of the colorbar/color legend. See \code{\link[plotly]{colorbar}} for more details.
#' 
#' @export
#' @examples
#' \dontrun{
#'
#' # mtcars
#' # x <- heatmapr(mtcars)
#' library(heatmaply)
#' heatmaply(iris[,-5], k_row = 3, k_col = 2)
#' heatmaply(cor(iris[,-5]))
#' heatmaply(cor(iris[,-5]), limits = c(-1,1))
#' heatmaply(mtcars, k_row = 3, k_col = 2)
#' # heatmaply(mtcars, k_row = 3, k_col = 2, grid_color = "white")
#' heatmaply(mtcars, k_row = 3, k_col = 2, grid_gap = 1)
#'
#' # make sure there is enough room for the labels:
#' heatmaply(mtcars, margins = c(40, 130))
#' # this is the same as using:
#' heatmaply(mtcars) %>% layout(margin = list(l = 130, b = 40))
#'
#' # control text angle
#' heatmaply(mtcars, column_text_angle = 90, margins = c(40, 130))
#' # the same as using srtCol:
#' # heatmaply(mtcars, srtCol = 90) %>% layout(margin = list(l = 130, b = 40))
#'
#'
#'
#' x <- mtcars
#' # different colors
#' heatmaply(x, colors = heat.colors(200))
#' # using special scale_fill_gradient_fun colors
#' heatmaply(x, scale_fill_gradient_fun = scale_color_gradient())
#'
#'
#' # We can join two heatmaps together:
#' library(heatmaply)
#' hm1 <- heatmaply(mtcars, margins = c(40, 130))
#' hm2 <- heatmaply(mtcars, scale = "col", margins = c(40, 130))
#' subplot(hm1, hm2, margin = .2)
#'
#' # If we want to share the Y axis, then it is risky to keep any of the dendrograms:
#' library(heatmaply)
#' hm1 <- heatmaply(mtcars, Colv = FALSE, Rowv = FALSE, margins = c(40, 130))
#' hm2 <- heatmaply(mtcars, scale = "col" , Colv = FALSE, Rowv = FALSE,
#'              margins = c(40, 130))
#' subplot(hm1, hm2, margin = .02, shareY = TRUE)
#'
#'
#' # We can save heatmaply as an HTML file by using:
#' heatmaply(iris[,-5], file = "heatmaply_iris.html")
#'
#' # If we don't want the HTML to be selfcontained, we can use the following:
#' library(heatmaply)
#' library(htmlwidgets)
#' heatmaply(iris[,-5]) %>%
#'    saveWidget(file="heatmaply_iris.html",selfcontained = FALSE)
#'
#'
#' # Example for using RowSideColors
#'
#' x  <- as.matrix(datasets::mtcars)
#' rc <- colorspace::rainbow_hcl(nrow(x))
#'
#' library(gplots)
#' library(viridis)
#' heatmap.2(x, trace = "none", col = viridis(100),
#'           RowSideColors=rc)
#'
#' heatmaply(x, seriate = "mean",
#'           RowSideColors=rc)
#'
#'
#' heatmaply(x[,-c(8,9)], seriate = "mean",
#'           col_side_colors = c(rep(0,5), rep(1,4)),
#'           row_side_colors = x[,8:9])
#' heatmaply(x[,-c(8,9)], seriate = "mean",
#'           col_side_colors = data.frame(a=c(rep(0,5), rep(1,4))),
#'           row_side_colors = x[,8:9])
#'
#'
#' ## Example of using Rowv And Colv for custumized dendrograms.
#'
#'
#' x  <- as.matrix(datasets::mtcars)
#'
#' # now let's spice up the dendrograms a bit:
#' library(dendextend)
#'
#' row_dend  <- x %>% dist %>% hclust %>% as.dendrogram %>%
#'   set("branches_k_color", k = 3) %>% set("branches_lwd", 4) %>%
#'   ladderize
#' #    rotate_DendSer(ser_weight = dist(x))
#' col_dend  <- x %>% t %>% dist %>% hclust %>% as.dendrogram %>%
#'   set("branches_k_color", k = 2) %>% set("branches_lwd", 4) %>%
#'   ladderize
#' #    rotate_DendSer(ser_weight = dist(t(x)))
#'
#' heatmaply(x, Rowv = row_dend, Colv = col_dend)
#'
#'
#' heatmaply(is.na10(airquality))
#' heatmaply(is.na10(airquality), grid_gap = 1)
#'
#' # grid_gap can handle quite large data matrix
#' heatmaply(matrix(1:10000,100,100), k_row = 3, k_col = 3, grid_gap = 1)
#'
#' # Examples of playing with font size:
#' heatmaply(mtcars, fontsize_col = 20, fontsize_row = 5, margin = c(100,90))
#'
#' }
heatmaply <- function(x, ...) {
  UseMethod("heatmaply")
}

#' @export
#' @rdname heatmaply
#' @importFrom assertthat assert_that
heatmaply.default <- function(x,
                              # elements for scale_fill_gradientn
                              colors = viridis(n=256, alpha = 1, begin = 0,
                                                end = 1, option = "viridis"),
                              limits = NULL,
                              na.value = "grey50",
                              row_text_angle = 0,
                              column_text_angle = 45,
                              subplot_margin = 0,
                              cellnote = NULL,
                              draw_cellnote = !is.null(cellnote),

                              ## dendrogram control
                              Rowv,
                              Colv,
                              distfun = dist,
                              hclustfun = hclust,
                              dist_method = NULL,
                              hclust_method = NULL,

                              distfun_row,
                              hclustfun_row,
                              distfun_col,
                              hclustfun_col,

                              dendrogram = c("both", "row", "column", "none"),
                              reorderfun = function(d, w) reorder(d, w),

                              k_row = 1,
                              k_col = 1,

                              symm = FALSE,
                              revC,

                              ## data scaling
                              scale = c("none", "row", "column"),
                              na.rm = TRUE,

                              row_dend_left = FALSE,
                              margins = c(NA, NA, NA, NA),
                              ...,
                              scale_fill_gradient_fun = scale_fill_gradientn(
                                colors = if(is.function(colors)) colors(256) else colors,
                                na.value = na.value, limits = limits),
                              grid_color = NA,
                              grid_gap = 0,
                              srtRow, srtCol,
                              xlab = "", ylab = "",
                              main = "",
                              titleX = TRUE, titleY = TRUE,
                              hide_colorbar = FALSE,
                              key.title = NULL,
                              return_ppxpy = FALSE,
                              row_side_colors,
                              row_side_palette,
                              col_side_colors,
                              col_side_palette,
                              ColSideColors = NULL,
                              RowSideColors = NULL,
                              seriate = c("OLO", "mean", "none", "GW"),
                              heatmap_layers = NULL,
                              branches_lwd = 0.6,
                              file,
                              long_data,
                              plot_method = c("ggplot", "plotly"),
                              label_names = c("row", "column", "value"),
                              fontsize_row = 10,
                              fontsize_col = 10,
                              cexRow, cexCol,
                              subplot_widths = NULL,
                              subplot_heights = NULL,
                              colorbar_len = 0.3,
                              colorbar_xanchor = "left",
                              colorbar_yanchor = "bottom",
                              colorbar_xpos = 1.1,
                              colorbar_ypos = 1,
                              col) {

  if (!missing(long_data)) {
    if (!missing(x)) warning("x and long_data should not be used together")
    assert_that(
      ncol(long_data) == 3,
      all(colnames(long_data) == c("name", "variable", "value"))
    )
    x <- reshape2::dcast(long_data, name ~ variable)
    rownames(x) <- x$name
    x$name <- NULL
  }

  # this is to fix the error: "argument * matches multiple formal arguments"
  if(!missing(col)) colors <- col

  plot_method <- match.arg(plot_method)
  dendrogram <- match.arg(dendrogram)

  if(!(is.data.frame(x) | is.matrix(x))) stop("x must be either a data.frame or a matrix.")

  if(!missing(srtRow)) row_text_angle <- srtRow
  if(!missing(srtCol)) column_text_angle <- srtCol

  if (!is.null(ColSideColors)) {
    col_side_colors <- ColSideColors
  }
  if (!is.null(RowSideColors)) {
    row_side_colors <- RowSideColors
  }

  if (!missing(cexRow)) fontsize_row <- cexRow
  if (!missing(cexCol)) fontsize_col <- cexCol

  # TODO: maybe create heatmaply.data.frame heatmaply.matrix instead.
  #       But right now I am not sure this would be needed.
  if(is.data.frame(x)) {
    ss_c_numeric <- sapply(x, is.numeric)
  }
  if(is.matrix(x)) {
    ss_c_numeric <- apply(x, 2, is.numeric)
  }

  # We must have some numeric values to be able to make a heatmap
  if(!any(ss_c_numeric)) stop("heatmaply only works for data.frame/matrix which includes some numeric columns.")

  # If we have non-numeric columns, we should move them to row_side_colors
  # TODO: add a parameter to control removing of non-numeric columns without moving them to row_side_colors
  if(!all(ss_c_numeric)) {
    row_side_colors <- if (missing(row_side_colors)) {
      data.frame(x[, !ss_c_numeric, drop= FALSE])
    } else {
      data.frame(row_side_colors, x[, !ss_c_numeric, drop= FALSE])
    }
    x <- x[, ss_c_numeric]
  }

  # help dendrogram work again:
  if(dendrogram == "row") Colv <- FALSE
  if(dendrogram == "column") Rowv <- FALSE
  if(dendrogram == "none") Rowv <- Colv <- FALSE

  # this also occurs in heatmapr, so it may be o.k. to remove the following line.
  seriate <- match.arg(seriate)

  hm <- heatmapr(x,
    row_side_colors = row_side_colors,
    col_side_colors = col_side_colors,
    seriate = seriate,

    cellnote = cellnote,

    ## dendrogram control
    Rowv = Rowv,
    Colv = Colv,
    distfun = distfun,
    hclustfun = hclustfun,
    dist_method = dist_method,
    hclust_method = hclust_method,

    distfun_row = distfun_row,
    hclustfun_row = hclustfun_row,
    distfun_col = distfun_col,
    hclustfun_col = hclustfun_col,

    dendrogram = dendrogram,
    reorderfun = reorderfun,

    k_row = k_row,
    k_col = k_col,

    symm = symm,
    revC = revC,

    ## data scaling
    scale = scale,
    na.rm = na.rm,

    ...)
  hmly <- heatmaply.heatmapr(hm, colors = colors, limits = limits,
                     scale_fill_gradient_fun = scale_fill_gradient_fun,
                     grid_color = grid_color,
                     grid_gap = grid_gap,
                     row_text_angle = row_text_angle,
                     column_text_angle = column_text_angle,
                     subplot_margin = subplot_margin,
                     row_dend_left = row_dend_left,
                     xlab = xlab, ylab = ylab, main = main,
                     titleX = titleX, titleY = titleY,
                     hide_colorbar = hide_colorbar,
                     key.title = key.title,
                     return_ppxpy = return_ppxpy,
                     margins = margins,
                     row_side_colors = row_side_colors,
                     row_side_palette = row_side_palette,
                     col_side_colors = col_side_colors,
                     col_side_palette = col_side_palette,
                     heatmap_layers = heatmap_layers,
                     ColSideColors = ColSideColors,
                     RowSideColors = RowSideColors,
                     branches_lwd = branches_lwd,
                     label_names = label_names,
                     plot_method = plot_method,
                     draw_cellnote = draw_cellnote,
                     fontsize_row = fontsize_row,
                     fontsize_col = fontsize_col,
                     subplot_widths = subplot_widths,
                     subplot_heights = subplot_heights,
                     colorbar_len = colorbar_len,
                     colorbar_xanchor = colorbar_xanchor,
                     colorbar_yanchor = colorbar_yanchor,
                     colorbar_xpos = colorbar_xpos,
                     colorbar_ypos = colorbar_ypos)

                     # TODO: think more on what should be passed in "..."

  if(!missing(file)) hmly %>% saveWidget(file = file, selfcontained = TRUE)

  hmly
}










heatmap_subplot_from_ggplotly <- function(p, px, py, pr, pc,
                                          row_dend_left = FALSE, subplot_margin = 0,
                                          titleX = TRUE, titleY = TRUE,
                                          widths=NULL, heights=NULL,
                                          plot_method, ...) {

  if (is.null(widths)) {
    if (!is.null(px)) {
      if (is.null(pr)) {
        widths <- c(0.8, 0.2)
      } else {
        widths <- c(0.7, 0.05, 0.2)
      }
    } else {
      if (is.null(pr)) {
        widths <- 1
      } else {
        widths <- c(0.9, 0.1)
      }
    }
  }

  if (is.null(heights)) {
    if (!is.null(py)) {
      if (is.null(pc)) {
        heights <- c(0.2, 0.8)
      } else {
        heights <- c(0.2, 0.05, 0.7)
      }
    } else {
      if (is.null(pc)) {
        heights <- 1
      } else {
        heights <- c(0.1, 0.9)
      }
    }
  }

  # make different plots based on which dendrogram and sidecolors we have
  row1_list <- list(py, plotly_empty(), plotly_empty())
  row2_list <- list(pc, plotly_empty(), plotly_empty())
  row3_list <- list(p, pr, px)

  if (row_dend_left) {
    row3_list <- rev(row3_list)
    row2_list <- rev(row2_list)
    row1_list <- rev(row1_list)
  }
  plots <- c(row1_list,
             row2_list,
             row3_list)

  column_list <- list(py, pc, p)
  ind_null_col <- sapply(column_list, is.null)
  ## number of rows depends on vertically aligned components
  nrows <- sum(!ind_null_col)
  ind_remove_col <- rep(ind_null_col, each = length(plots) / 3)

  ind_null_row <- sapply(row3_list, is.null)
  ind_remove_row <- rep(ind_null_row, length.out = length(plots))


  if (sum(!ind_null_col) != length(heights)) {
    stop(paste("Number of subplot_heights supplied is not correct; should be",
      sum(!ind_null_col), "but is", length(heights)))
  }
  if (sum(!ind_null_row) != length(widths)) {
    stop(paste("Number of subplot_widths supplied is not correct; should be",
      sum(!ind_null_row), "but is", length(widths)))
  }

  ## Remove all null plots
  plots <- plots[!(ind_remove_row | ind_remove_col)]


  ## Interim solution before removing warnings in documented way
  suppressMessages(
    suppressWarnings(
      s <- subplot(plots,
        nrows = nrows,
        widths = if(row_dend_left) rev(widths) else widths,
        shareX = TRUE, shareY = TRUE,
        titleX = titleX, titleY = titleY,
        margin = subplot_margin,
        heights = heights)
      )
  )

  if (plot_method == "plotly") {
    if (row_dend_left) {
      num_rows <- sum(!ind_null_row)
      str <- ifelse(num_rows > 1, num_rows, "")
      l <- list(
        anchor = paste0("x", str),
        side = "right",
        showticklabels=TRUE
      )
      num_cols <- sum(!ind_null_col)
      if (num_cols == 1) {
        lay <- function(p) layout(p, yaxis = l)
      } else if (num_cols == 2) {
        lay <- function(p) layout(p, yaxis2 = l)
      } else if (num_cols == 3) {
        lay <- function(p) layout(p, yaxis3 = l)
      }
      s <- lay(s)
    }
  }


  # s <- subplot(plots,
  #   nrows = nrows,
  #   widths = if(row_dend_left) rev(widths) else widths,
  #   shareX = TRUE, shareY = TRUE,
  #   titleX = titleX, titleY = titleY,
  #   margin = subplot_margin,
  #   heights = heights)


  return(s)
}

#' @export
#' @rdname heatmaply
heatmaply.heatmapr <- function(x,
                               # elements for scale_fill_gradientn
                               colors = viridis(n=256, alpha = 1, begin = 0,
                                                    end = 1, option = "viridis"),
                               limits = NULL,
                               na.value = "grey50",
                               row_text_angle = 0,
                               column_text_angle = 45,
                               subplot_margin = 0,

                               row_dend_left = FALSE,
                               margins = c(NA, NA, NA, NA),
                               ...,
                               scale_fill_gradient_fun = scale_fill_gradientn(
                                 colors = if(is.function(colors)) colors(256) else colors,
                                 na.value = na.value, limits = limits),
                               grid_color = NA,
                               grid_gap = 0,
                               srtRow, srtCol,
                               xlab = "", ylab = "",
                               main = "",
                               titleX = TRUE, titleY = TRUE,
                               hide_colorbar = FALSE,
                               key.title = NULL,
                               return_ppxpy = FALSE,
                               draw_cellnote = FALSE,
                               row_side_colors,
                               row_side_palette,
                               col_side_colors,
                               col_side_palette,
                               plot_method = c("ggplot", "plotly"),
                               ColSideColors,
                               RowSideColors,
                               heatmap_layers = NULL,
                               branches_lwd = 0.6,
                               label_names,
                               fontsize_row = 10,
                               fontsize_col = 10,
                               subplot_widths = NULL,
                               subplot_heights = NULL,
                               colorbar_xanchor = if(row_dend_left) "right" else "left",
                               colorbar_yanchor = "top",
                               colorbar_xpos = if(row_dend_left) -0.1 else 1.1,
                               colorbar_ypos = 0,
                               colorbar_len = 0.3) {

  plot_method <- match.arg(plot_method)

  # informative errors for mis-specified limits
  if(!is.null(limits)) {
    if(!is.numeric(limits)) stop("limits must be numeric")
    if(length(limits) != 2L) stop("limits must be of length 2 (i.e.: two dimensional)")

    r <- range(as.matrix(x$matrix$data), na.rm = TRUE)
    limits <- sort(limits)

    ## Warn for broken heatmap colors
    if (limits[1] > r[1]) {
      limits[1] <- r[1]
      warning("Lower limit is not <= lowest value in x, min of limits is set to the min of the range (otherwise, colors will be broken!)")
    }
    if (limits[2] < r[2]) {
      limits[2] <- r[2]
      warning("Upper limit is not >= highest value in x, max of limits is set to the max of the range (otherwise, colors will be broken!)")
    }
  }
  if(!missing(srtRow)) row_text_angle <- srtRow
  if(!missing(srtCol)) column_text_angle <- srtCol

  # x is a heatmapr object.
  # heatmapr <- list(rows = rowDend, cols = colDend, matrix = mtx, image = imgUri,
  #                  theme = theme, options = options)
  # x <- heatmapr(mtcars)
  # source: http://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
  theme_clear_grid_dends <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  # dendrograms:
  rows <- x$rows
  cols <- x$cols

  if(!is.null(branches_lwd) && branches_lwd != 1) {
    if(is.dendrogram(rows) && !has_edgePar(rows, "lwd")) rows <- set(rows, "branches_lwd", branches_lwd)
    if(is.dendrogram(cols) && !has_edgePar(cols, "lwd")) cols <- set(cols, "branches_lwd", branches_lwd)
  }



  # this is using dendextend
  if(is.null(cols)) {
    py <- NULL
  } else {
    if (plot_method == "ggplot") {
      col_ggdend <- as.ggdend(cols)
      xlims <- c(0.5, nrow(col_ggdend$labels) + 0.5)
      py <- ggplot(cols, labels  = FALSE) + theme_bw() +
        coord_cartesian(expand = FALSE, xlim = xlims) +
        theme_clear_grid_dends
    } else {
      suppressWarnings(      py <- plotly_dend(cols, side = "col"))
    }
  }
  if(is.null(rows)) {
    px <- NULL
  } else {
    if (plot_method == "ggplot") {
      row_ggdend <- as.ggdend(rows)
      ylims <- c(0.5, nrow(row_ggdend$labels) + 0.5)

      px <- ggplot(row_ggdend, labels  = FALSE) +
        # coord_cartesian(expand = FALSE) +
        coord_flip(expand = FALSE, xlim = ylims) +
        theme_bw() +
        theme_clear_grid_dends
      if(row_dend_left) px <- px + scale_y_reverse()
    } else {
      px <- plotly_dend(rows, flip = row_dend_left, side = "row")
    }
  }
  # create the heatmap
  data_mat <- x$matrix$data

  if (plot_method == "ggplot") {
    p <- ggplot_heatmap(data_mat,
                      row_text_angle,
                      column_text_angle,
                      scale_fill_gradient_fun,
                      grid_color,
                      cellnote = x$matrix$cellnote,
                      draw_cellnote = draw_cellnote,
                      key.title = key.title,
                      layers = heatmap_layers,
                      row_dend_left = row_dend_left,
                      label_names = label_names,
                      fontsize_row = fontsize_row, fontsize_col = fontsize_col)
  } else if (plot_method == "plotly") {

    p <- plotly_heatmap(data_mat, limits = limits, colors = colors,
      row_text_angle = row_text_angle, column_text_angle = column_text_angle,
      fontsize_row = fontsize_row, fontsize_col = fontsize_col,
      colorbar_yanchor = colorbar_yanchor, colorbar_xanchor = colorbar_xanchor,
      colorbar_xpos = colorbar_xpos, colorbar_ypos = colorbar_ypos, 
      colorbar_len = colorbar_len)
  }


  # TODO: Add native plotly sidecolor function.
  # TODO: Possibly use function to generate all 3 plots to prevent complex logic here
  if (missing(row_side_colors)) {
    pr <- NULL
  } else {
    side_color_df <- x[["row_side_colors"]]
    if (is.matrix(side_color_df)) side_color_df <- as.data.frame(side_color_df)
    assert_that(
      nrow(side_color_df) == nrow(data_mat),
      is.data.frame(side_color_df)
    )
    pr <- side_color_plot(x[["row_side_colors"]], type = "row",
      text_angle = column_text_angle, 
      palette = row_side_palette,
      is_colors = !is.null(RowSideColors), label_name = label_names[[1]])
  }

  if (missing(col_side_colors)) {
    pc <- NULL
  } else {
    warning("The hover text for col_side_colors is currently not implemented (due to an issue in plotly). We hope this would get resolved in future releases.")

    side_color_df <- x[["col_side_colors"]]
    if (is.matrix(side_color_df)) side_color_df <- as.data.frame(side_color_df)
    assert_that(
      nrow(side_color_df) == ncol(data_mat),
      is.data.frame(side_color_df)
    )
    ## Just make sure it's character first
    side_color_df[] <- lapply(side_color_df, as.character)
    pc <- side_color_plot(side_color_df, type = "column",
      text_angle = row_text_angle, 
      palette = col_side_palette,
      is_colors = !is.null(ColSideColors),
      label_name = label_names[[2]]
    )
  }

  if(return_ppxpy) {
    return(list(p=p, px=px, py=py, pr=pr, pc=pc))
  }

  ## plotly:
  # turn p, px, and py to plotly objects if necessary
  if (!is.plotly(p)) p <- ggplotly(p) %>% layout(showlegend=FALSE)
  if (!is.null(px) && !is.plotly(px)) {
    px <- ggplotly(px, tooltip = "y") %>%
      layout(showlegend=FALSE)
  }
  if (!is.null(py) && !is.plotly(py)) {
    py <- ggplotly(py, tooltip = "y") %>%
      layout(showlegend=FALSE)
  }

  # https://plot.ly/r/reference/#Layout_and_layout_style_objects
  p <- layout(p,              # all of layout's properties: /r/reference/#layout
              title = main, # layout's title: /r/reference/#layout-title
              xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = xlab     # xaxis's title: /r/reference/#layout-xaxis-title
                # showgrid = T        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
              ),
              yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = ylab      # yaxis's title: /r/reference/#layout-yaxis-title
              ))
  if (hide_colorbar) {
    p <- hide_colorbar(p)
    # px <- hide_colorbar(px)
    # py <- hide_colorbar(py)
  }

  # Adjust top based on whether main is empty or not.
  if (is.na(margins[3])) margins[3] <- ifelse(main == "", 0, 30)
  
  
  min_marg_row <- calc_margin(rownames(data_mat), 
    fontsize = p$x$layout$yaxis$tickfont$size)
  if (row_dend_left && is.na(margins[4])) {
    margins[4] <- min_marg_row
  } else if (!row_dend_left && is.na(margins[2])) {
    margins[2] <- min_marg_row  
  }
  if (is.na(margins[1])) {
    margins[1] <- calc_margin(colnames(data_mat), 
        fontsize = p$x$layout$yaxis$tickfont$size)
  }
   
  # add a white grid
  if(grid_gap > 0) {
    p <- style(p, xgap = grid_gap, ygap = grid_gap)
    # doesn't seem to work.
    # if(!is.null(pr)) pr <- style(pr, xgap = grid_gap)
    # if(!is.null(pc)) pc <- style(pc, ygap = grid_gap)
  }


  heatmap_subplot <- heatmap_subplot_from_ggplotly(p = p, px = px, py = py,
    row_dend_left = row_dend_left, subplot_margin = subplot_margin,
    titleX = titleX, titleY = titleY, pr = pr, pc = pc, plot_method = plot_method)
  l <- layout(heatmap_subplot,
      margin = list(l = margins[2], b = margins[1], t = margins[3], r = margins[4]),
      legend = list(y=1, yanchor="top")
    )

  # keep only relevant plotly options
  l <- config(l, displaylogo = FALSE, collaborate = FALSE,
        modeBarButtonsToRemove = c("sendDataToCloud", "select2d", "lasso2d","autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "sendDataToCloud"))

  l

}

calc_margin <- function(labels, fontsize) {
    max(nchar(labels) * fontsize, na.rm = TRUE)
}
