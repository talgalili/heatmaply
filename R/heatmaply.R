
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
#' @param limits a two dimensional numeric vector specifying the data range for the scale.
#' @param na.value color to use for missing values (default is "grey50").
#' @param scale_fill_gradient_fun A function that creates a smooth gradient for the heatmap.
#' The default uses \link[ggplot2]{scale_fill_gradientn} with the values of colors, limits, and
#' na.value that are supplied by the user. The user can input a customized function, such as
#' \link{scale_colour_gradient}() in order to get other results (although the virids default
#' is quite recommended)
#'
#' @param row_dend_left logical (default is FALSE). Should the row dendrogram be
#' plotted on the left side of the heatmap. If false then it will be plotted on the right
#' side.
#'
#' @param ... other parameters passed to \link{heatmapr} (currently, various parameters may be ignored.
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
#'
#' x <- mtcars
#' # different colors
#' heatmaply(x, colors = heat.colors(200))
#' # using special scale_fill_gradient_fun colors
#' heatmaply(x, scale_fill_gradient_fun = scale_colour_gradient())
#'
#' }
heatmaply <- function(x,
                      # elements for scale_fill_gradientn
                      colors = viridis(n=256, alpha = 1, begin = 0,
                                        end = 1, option = "viridis"),
                      limits = NULL,
                      na.value = "grey50",
                      scale_fill_gradient_fun =
                        scale_fill_gradientn(colors = colors,
                                               na.value = na.value, limits = limits),
                      row_dend_left = FALSE,
                      ...) {
  UseMethod("heatmaply")
}

#' @export
heatmaply.default <- function(x,
                              # elements for scale_fill_gradientn
                              colors = viridis(n=256, alpha = 1, begin = 0,
                                                end = 1, option = "viridis"),
                              limits = NULL,
                              na.value = "grey50",
                              scale_fill_gradient_fun =
                                scale_fill_gradientn(colors = colors,
                                                       na.value = na.value, limits = limits),
                              row_dend_left = FALSE,
                              ...) {
  hm <- heatmapr(x, ...)
  heatmaply.heatmapr(hm, # colors = colors, limits = limits,
                     scale_fill_gradient_fun = scale_fill_gradient_fun,
                     row_dend_left = row_dend_left) # TODO: think more on what should be passed in "..."
}


#' @export
heatmaply.heatmapr <- function(x,
                               # elements for scale_fill_gradientn
                               colors = viridis(n=256, alpha = 1, begin = 0,
                                                    end = 1, option = "viridis"),
                               limits = NULL,
                               na.value = "grey50",
                               scale_fill_gradient_fun =
                                 scale_fill_gradientn(colors = colors,
                                                        na.value = na.value, limits = limits),
                               row_dend_left = FALSE,
                               ...) {


  # informative errors for mis-specified limits
  if(!is.null(limits)) {
    if(!is.numeric(limits)) stop("limits must be numeric")
    if(length(limits) != 2L) stop("limits must be of length 2 (i.e.: two dimensional)")
  }

  # x is a heatmapr object.

  # heatmapr <- list(rows = rowDend, cols = colDend, matrix = mtx, image = imgUri,
  #                  theme = theme, options = options)


  # TODO: we assume the creation of dendrograms. Other defaults should be made when the user
  #     chooses to not work with dendrograms.

  # x <- heatmapr(mtcars)

  theme_clear_grid_heatmap <- theme(axis.line = element_line(colour = "black"),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            panel.background = element_blank())

  # source: http://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
  theme_clear_grid_dends <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


  rows <- x$rows
  cols <- x$cols

  # heatmap
  xx <- x$matrix$data
  df <- as.data.frame(xx)
  colnames(df) <- x$matrix$cols
  df$row_name <- x$matrix$rows
  df$row_name <- with(df, factor(row_name, levels=row_name, ordered=TRUE))
  mdf <- reshape2::melt(df, id.vars="row_name")

  # dendrograms
  # this is using dendextend
  px <- ggplot(rows, labels  = FALSE) + coord_flip()+ theme_bw() + theme_clear_grid_dends
  py <- ggplot(cols, labels  = FALSE) + theme_bw() + theme_clear_grid_dends

  if(row_dend_left) px <- px + scale_y_reverse()

  px <- ggplotly(px, tooltip = "")
  py <- ggplotly(py, tooltip = "")


  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes(x = variable, y = row_name)) + geom_tile(aes(fill = value)) +
    # scale_fill_viridis() +
    scale_fill_gradient_fun +
    theme_bw()+ theme_clear_grid_heatmap
  # p <- plot_ly(z = xx, type = "heatmap")
  # ggplotly(p) # works great

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

  if(row_dend_left) {
    s <- subplot(plotly_empty(), py, px, p, nrows = 2, widths = c(.2,.8), heights = c(.2,.8), margin = 0,
                 shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
  } else {
    # row dend on the right side
    s <- subplot(py, plotly_empty(), p, px, nrows = 2, widths = c(.8,.2), heights = c(.2,.8), margin = 0,
                 shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
  }

  l <- layout(s, showlegend = FALSE)
  print(l)

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
    scale_fill_viridis() + theme_bw()
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
               shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)

  layout(s, showlegend = FALSE)


}
