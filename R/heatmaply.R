# devtools::install_github("ropensci/plotly", ref = "fix/subplot")

# reference: https://plot.ly/ggplot2/ggdendro-dendrograms/
# to answer later: http://stackoverflow.com/questions/34733829/questions-about-a-tutorial-regarding-interactive-heatmaps-with-plotly
# to check: https://plot.ly/r/heatmaps/



#' @title  Creates a plotly heatmap
#'
#' @description
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#' @param x can either be a heatmapr object, or a numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#'
#' @param colors a vector of colors to use for heatmap color.
#' The default uses \code{\link[viridis]{viridis}(n=256, alpha = 1, begin = 0, end = 1, option = "viridis")}
#' It is passed to \link[ggplot2]{scale_fill_gradientn}.
#' If colors is a color function (with the first argument being `n` = the number of colors),
#' it will be used to create 256 colors from that function.
#' @param limits a two dimensional numeric vector specifying the data range for the scale.
#' @param na.value color to use for missing values (default is "grey50").
#'
#' @param row_text_angle numeric (Default is 0), the angle of the text of the rows. (this is called srtRow in \link[gplots]{heatmap.2})
#' @param column_text_angle numeric (Default is 45), the angle of the text of the columns. (this is called srtCol in \link[gplots]{heatmap.2})
#'
#' @param subplot_margin Currently not well implemented. It is passed to \link[plotly]{subplot}. Default is 0. Either a single value or
#'  four values (all between 0 and 1). If four values are provided,
#'  the first is used as the left margin, the second is used as the right margin,
#'  the third is used as the top margin, and the fourth is used as the bottom margin.
#'  If a single value is provided, it will be used as all four margins.
#'
#' @param row_dend_left logical (default is FALSE). Should the row dendrogram be
#' plotted on the left side of the heatmap. If false then it will be plotted on the right
#' side.
#'
#' @param margins numeric vector of length 2 (default is c(50,50)) containing the margins (see \link[plotly]{layout}) for column and row names, respectively.
#'
#' @param ... other parameters passed to \link{heatmapr} (currently, various parameters may be ignored.
#'
#' @param scale_fill_gradient_fun A function that creates a smooth gradient for the heatmap.
#' The default uses \link[ggplot2]{scale_fill_gradientn} with the values of colors, limits, and
#' na.value that are supplied by the user. The user can input a customized function, such as
#' \link{scale_colour_gradient}() in order to get other results (although the virids default
#' is quite recommended)
#'
#' @param grid_color control the color of the heatmap grid. Default is NA. Value passed to \link[ggplot2]{geom_tile}.
#' (another parameter, grid_color, will be added in the future - once it is implemented in plotly)
#'
#' @param srtRow if supplied, this overrides row_text_angle (this is to stay compatible with \link[gplots]{heatmap.2})
#' @param srtCol if supplied, this overrides column_text_angle (this is to stay compatible with \link[gplots]{heatmap.2})
#'
#' @param xlab A character title for the x axis.
#' @param ylab A character title for the y axis.
#'
#' @param titleX logical (TRUE). should x-axis titles be retained? (passed to \link[plotly]{subplot}).
#' @param titleY logical (TRUE). should y-axis titles be retained? (passed to \link[plotly]{subplot}).
#'
#' @param hide_colorbar logical (FALSE). If TRUE, then the color bar is hidden.
#'
#' @param key.title (character) main title of the color key. If set to NULL (default) no title will be plotted.
#'
#' @param return_ppxpy logical (FALSE). If TRUE, then no plotting is done and the p, px and py objects are
#' returned (before turning into plotly objects). This is a temporary option which might be removed in the
#' future just to make it easy to create a ggplot heatmaps.
#'
#' Please submit an issue on github if you have a feature that you wish to have added)
#' @aliases
#' heatmaply.default
#' heatmaply.heatmapr
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
#' heatmaply(mtcars, k_row = 3, k_col = 2, grid_color = "white")
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
#' heatmaply(x, scale_fill_gradient_fun = scale_colour_gradient())
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
#' # We can save heatmaply as a widget by using:
#' library(heatmaply)
#' library(htmlwidgets)
#' heatmaply(iris[,-5]) %>%
#'    saveWidget(file="test.html",selfcontained = FALSE)
#'
#'
#'
#' }
heatmaply <- function(x,
                      # elements for scale_fill_gradientn
                      colors = viridis(n=256, alpha = 1, begin = 0,
                                        end = 1, option = "viridis"),
                      limits = NULL,
                      na.value = "grey50",
                      row_text_angle = 0,
                      column_text_angle = 45,
                      subplot_margin = 0,
                      row_dend_left = FALSE,
                      margins = c(50, 50),
                      ...,
                      scale_fill_gradient_fun = scale_fill_gradientn(
                          colors = if(is.function(colors)) colors(256) else colors,
                                             na.value = na.value, limits = limits),
                      grid_color = NA,
                      srtRow, srtCol,
                      xlab = "", ylab = "",
                      titleX = TRUE, titleY = TRUE,
                      hide_colorbar = FALSE,
                      key.title = NULL,
                      return_ppxpy = FALSE

                      ) {
  UseMethod("heatmaply")
}

#' @export
heatmaply.default <- function(x,
                              # elements for scale_fill_gradientn
                              colors = viridis(n=256, alpha = 1, begin = 0,
                                                end = 1, option = "viridis"),
                              limits = NULL,
                              na.value = "grey50",

                              row_text_angle = 0,
                              column_text_angle = 45,
                              subplot_margin = 0,
                              row_dend_left = FALSE,
                              margins = c(50, 50),
                              ...,
                              scale_fill_gradient_fun = scale_fill_gradientn(
                                colors = if(is.function(colors)) colors(256) else colors,
                                na.value = na.value, limits = limits),
                              grid_color = NA,
                              srtRow, srtCol,
                              xlab = "", ylab = "",
                              titleX = TRUE, titleY = TRUE,
                              hide_colorbar = FALSE,
                              key.title = NULL,
                              return_ppxpy = FALSE

                              ) {

  if(!missing(srtRow)) row_text_angle <- srtRow
  if(!missing(srtCol)) column_text_angle <- srtCol


  hm <- heatmapr(x, ...)
  heatmaply.heatmapr(hm, # colors = colors, limits = limits,
                     scale_fill_gradient_fun = scale_fill_gradient_fun,
                     grid_color = grid_color,
                     row_text_angle = row_text_angle,
                     column_text_angle = column_text_angle,
                     subplot_margin = subplot_margin,
                     row_dend_left = row_dend_left,
                     xlab=xlab, ylab=ylab,
                     titleX = titleX, titleY = titleY,
                     hide_colorbar = hide_colorbar,
                     key.title = key.title,
                     return_ppxpy = return_ppxpy,
                     margins = margins
                      ) # TODO: think more on what should be passed in "..."
}



# xx is a data matrix
ggplot_heatmap <- function(xx,
                           row_text_angle = 0,
                           column_text_angle = 45,
                           scale_fill_gradient_fun =
                             scale_fill_gradientn(colors = viridis(n=256, alpha = 1, begin = 0,
                                                                   end = 1, option = "viridis"),
                                                  na.value = "grey50", limits = NULL),
                           grid_color = NA,
                           grid_size = 0.5,
                           key.title = NULL,
                           ...) {

  theme_clear_grid_heatmap <- theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank())


  # heatmap
  # xx <- x$matrix$data
  df <- as.data.frame(xx)
  # colnames(df) <- x$matrix$cols
  df$row <- if(!is.null(rownames(xx)))
              {rownames(xx)} else
              {1:nrow(xx)}


  df$row <- with(df, factor(row, levels=row, ordered=TRUE))
  mdf <- reshape2::melt(df, id.vars="row")
  colnames(mdf)[2] <- "column" # rename "variable"

  # TODO:
  # http://stackoverflow.com/questions/15921799/draw-lines-around-specific-areas-in-geom-tile


  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes_string(x = "column", y = "row")) +
    geom_tile(aes_string(fill = "value"), color = grid_color, size = grid_size) +
    # scale_linetype_identity() +
    # scale_fill_viridis() +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient_fun +
    theme_bw()+ theme_clear_grid_heatmap +
    theme(axis.text.x = element_text(angle = column_text_angle, hjust = 1),
          axis.text.y = element_text(angle = row_text_angle, hjust = 1)
          )
  # p <- p + scale_x_discrete(limits = unique(mdf))

  # http://stats.stackexchange.com/questions/5007/how-can-i-change-the-title-of-a-legend-in-ggplot2
  p <- p + labs(fill=key.title)

  # until this bug is fixed: https://github.com/ropensci/plotly/issues/699
  # we are forced to use geom_hline and geom_vline
  if(!is.na(grid_color)) {
    p <- p + geom_hline(yintercept =c(0:nrow(xx))+.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly
    p <- p + geom_vline(xintercept =c(0:ncol(xx))+.5, color = grid_color) # , size = grid_size # not implemented since it doesn't work with plotly

  }



  p
}
#
# library(ggplot2)
# library(plotly)
# # library(heatmaply)
# ggplot_heatmap <- heatmaply:::ggplot_heatmap
# class_to <- function(x, new_class) {
#   class(x) <- new_class
#   x
# }
# na_mat <- function(x) {
#   x %>% is.na %>% class_to("numeric")
# }
#
# p <- heatmaply:::ggplot_heatmap(na_mat(airquality),
#                     scale_fill_gradient_fun = scale_fill_gradientn(colors= c("white","black")) ,
#                     grid_color = "grey", grid_size = 1)
# plot(p)
# ggplotly(p)
# p <- ggplot_heatmap(mtcars,
#                     grid_color = "white")
# p
#

heatmap_subplot_from_ggplotly <- function(p, px, py, top_corner, row_dend_left, subplot_margin = 0,
                                          titleX = TRUE, titleY = TRUE,
                                          widths = c(.8,.2), heights = c(.2,.8), ...) {

  # make different plots based on which dendrogram we have



  if(!is.null(px) & !is.null(py)) {
    # ------------- most of the time we use this: -------------
    # we have both dendrograms
    if(row_dend_left) {
      s <- subplot(top_corner, py, px, p, nrows = 2,
                   widths = rev(widths), heights = heights, margin = subplot_margin,
                   shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)
    } else {
      # row dend on the right side
      s <- subplot(py, top_corner, p, px, nrows = 2,
                   widths = widths, heights = heights, margin = subplot_margin,
                   shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)
    }

  } else {
    # we are missing SOME dendrogram (or both)

    if(is.null(px) & is.null(py)) {
      s <- subplot(p)
    }

    if(!is.null(py)) {
      # then px is NULL
      s <- subplot(py, p, nrows = 2,
                   heights = heights, margin = subplot_margin,
                   shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)
    }

    if(!is.null(px)) {
      # then py is NULL
      if(row_dend_left) {
        s <- subplot(px, p, nrows = 1,
                     widths = rev(widths), margin = subplot_margin,
                     shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)
      } else {
        # row dend on the right side
        s <- subplot(p, px, nrows = 1,
                     widths = widths, margin = subplot_margin,
                     shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)
      }
    }




  }


  return(s)


}





#' @export
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

                               margins = c(50, 50),
                               ...,
                               scale_fill_gradient_fun = scale_fill_gradientn(
                                 colors = if(is.function(colors)) colors(256) else colors,
                                 na.value = na.value, limits = limits),
                               grid_color = NA,
                               srtRow, srtCol,
                               xlab = "", ylab = "",
                               titleX = TRUE, titleY = TRUE,
                               hide_colorbar = FALSE,
                               key.title = NULL,
                               return_ppxpy = FALSE

                               ) {


  # informative errors for mis-specified limits
  if(!is.null(limits)) {
    if(!is.numeric(limits)) stop("limits must be numeric")
    if(length(limits) != 2L) stop("limits must be of length 2 (i.e.: two dimensional)")
  }

  if(!missing(srtRow)) row_text_angle <- srtRow
  if(!missing(srtCol)) column_text_angle <- srtCol


  # x is a heatmapr object.

  # heatmapr <- list(rows = rowDend, cols = colDend, matrix = mtx, image = imgUri,
  #                  theme = theme, options = options)


  # TODO: we assume the creation of dendrograms. Other defaults should be made when the user
  #     chooses to not work with dendrograms.

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
  # this is using dendextend
  if(is.null(cols)) {
    py <- NULL
  } else {
    py <- ggplot(cols, labels  = FALSE) + theme_bw() +
      coord_cartesian(expand = FALSE) +
      theme_clear_grid_dends
  }


  if(is.null(rows)) {
    px <- NULL
  } else {
    px <- ggplot(rows, labels  = FALSE) +
      # coord_cartesian(expand = FALSE) +
      coord_flip(expand = FALSE) + theme_bw() + theme_clear_grid_dends
    if(row_dend_left) px <- px + scale_y_reverse()
  }


  # create the heatmap

  data_mat <- x$matrix$data
  p <- ggplot_heatmap(data_mat,
                      row_text_angle,
                      column_text_angle,
                      scale_fill_gradient_fun,
                      grid_color,
                      key.title = key.title)


  if(return_ppxpy) {
    return(list(p=p, px=px, py=py))
  }



  ## plotly:
  # turn p, px, and py to plotly objects
  p <- ggplotly(p)
  if(!is.null(px)) px <- ggplotly(px, tooltip = "y")
  if(!is.null(py)) py <- ggplotly(py, tooltip = "y")




  # https://plot.ly/r/reference/#Layout_and_layout_style_objects
  p <- layout(p,              # all of layout's properties: /r/reference/#layout
              # title = "unemployment", # layout's title: /r/reference/#layout-title
              xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = xlab     # xaxis's title: /r/reference/#layout-xaxis-title
                # showgrid = T        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
              ),
              yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = ylab      # yaxis's title: /r/reference/#layout-yaxis-title
              ))


  if(hide_colorbar) {
    p <- hide_colorbar(p)
    # px <- hide_colorbar(px)
    # py <- hide_colorbar(py)

    }

  # TODO: this doesn't work because of the allignment. But using this might
  # speedup the code to deal with MUCH larger matrices.
  # p <- plot_ly(z = xx, type = "heatmap")


  # p <- plot_ly(z = xx, type = "heatmap")
  # ggplotly(p) # works great

  # source for: theme(axis.text.x = element_text(angle = column_text_angle, hjust = 1))
  # http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2


  # if(row_dend_left) p <- p + scale_y_reverse()



  # # hide axis ticks and grid lines
  # eaxis <- list(
  #   showticklabels = FALSE,
  #   showgrid = FALSE,
  #   zeroline = FALSE
  # )

  # p_empty <- plot_ly() %>%
  #   # note that margin applies to entire plot, so we can
  #   # add it here to make tick labels more readable
  #   layout(margin = list(l = 200),
  #          xaxis = eaxis,
  #          yaxis = eaxis)

  top_corner <- plotly_empty()
  # top_corner <- ggplotly(qplot(as.numeric(xx), geom="histogram"))

  # create the subplot
  heatmap_subplot <- heatmap_subplot_from_ggplotly(p, px, py, top_corner, row_dend_left, subplot_margin,
                                                   titleX = titleX, titleY = titleY)



  l <- layout(heatmap_subplot, showlegend = FALSE)  %>%
    layout(margin = list(l = margins[2], b = margins[1]))

  # print(l)
  l
}

# theme_set(theme_cowplot())
# library(cowplot)
# require2(cowplot)

#




if(FALSE) {
  # devtools::install_github("ropensci/plotly", ref = "fix/subplot")
  # devtools::install_github('talgalili/heatmaply')


  library(ggplot2)
  library(dendextend)
  library(plotly)
  library(viridis)

  #dendogram data
  x <- as.matrix(scale(mtcars))
  dd.col <- as.dendrogram(hclust(dist(x)))
  dd.row <- as.dendrogram(hclust(dist(t(x))))
  dd.col <- color_branches(dd.col, k = 3)
  dd.row <- color_branches(dd.row, k = 2)

  px <- ggplot(dd.row, labels  = FALSE) + theme_bw()
  py <- ggplot(dd.col, labels  = FALSE) + coord_flip()+ theme_bw()

  # heatmap
  col.ord <- order.dendrogram(dd.col)
  row.ord <- order.dendrogram(dd.row)
  xx <- scale(mtcars)[col.ord, row.ord]
  xx_names <- attr(xx, "dimnames")
  df <- as.data.frame(xx)
  colnames(df) <- xx_names[[2]]
  df$car <- xx_names[[1]]
  df$car <- with(df, factor(car, levels=car, ordered=TRUE))
  mdf <- reshape2::melt(df, id.vars="car")

  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes(x = variable, y = car)) + geom_tile(aes(fill = value)) +
    scale_fill_viridis() + theme_bw() +
    theme(axis.text.x = element_text(angle = column_text_angle, hjust = 1))
  # p <- plot_ly(z = xx, type = "heatmap")
  # ggplotly(p) # works great
  # ggplotly(p, tooltip = "none")
  # ggplotly(px, tooltip = "")

  #


  # # hide axis ticks and grid lines
  # eaxis <- list(
  #   showticklabels = FALSE,
  #   showgrid = FALSE,
  #   zeroline = FALSE
  # )

  # p_empty <- plot_ly() %>%
  #   # note that margin applies to entire plot, so we can
  #   # add it here to make tick labels more readable
  #   layout(margin = list(l = 200),
  #          xaxis = eaxis,
  #          yaxis = eaxis)

  s <- subplot(px, plotly_empty(), p, py, nrows = 2, widths = c(.8,.2), heights = c(.2,.8), margin = 0,
               shareX = TRUE, shareY = TRUE, titleX = titleX, titleY = titleY)

  layout(s, showlegend = FALSE)


}



