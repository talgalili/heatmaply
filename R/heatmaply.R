
# devtools::install_github("ropensci/plotly", ref = "fix/subplot")

# reference: https://plot.ly/ggplot2/ggdendro-dendrograms/
# to answer later: http://stackoverflow.com/questions/34733829/questions-about-a-tutorial-regarding-interactive-heatmaps-with-plotly
# to check: https://plot.ly/r/heatmaps/


#' Creates a plotly heatmap
#'
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#' @param x can either be a heatmapr object, or a numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # mtcars
#' # x <- heatmapr(mtcars)
#' library(heatmaply)
#' heatmaply(iris[,-5], k_row = 3, k_col = 2)
#' heatmaply(mtcars, k_row = 3, k_col = 2)
#' }
heatmaply <- function(x,...) {
  UseMethod("heatmaply")
}

#' @export
heatmaply.default <- function(x, ...) {
  hm <- heatmapr(x, ...)
  heatmaply(hm) # TODO: think more on what should be passed in "..."
}


#' @export
heatmaply.heatmapr <- function(x, ...) {
  # x is a heatmapr object.

  # heatmapr <- list(rows = rowDend, cols = colDend, matrix = mtx, image = imgUri,
  #                  theme = theme, options = options)


  # TODO: we assume the creation of dendrograms. Other defaults should be made when the user
  #     chooses to not work with dendrograms.

  cols <- x$rows
  rows <- x$cols

  # heatmap
  xx <- x$matrix$data
  df <- as.data.frame(xx)
  colnames(df) <- x$matrix$cols
  df$row_name <- x$matrix$rows
  df$row_name <- with(df, factor(row_name, levels=row_name, ordered=TRUE))
  mdf <- reshape2::melt(df, id.vars="row_name")

  # dendrograms
  # this is using dendextend
  px <- ggplot(rows, labels  = FALSE) + theme_bw()
  py <- ggplot(cols, labels  = FALSE) + coord_flip()+ theme_bw()


  # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  p <- ggplot(mdf, aes(x = variable, y = row_name)) + geom_tile(aes(fill = value)) +
    scale_fill_viridis() + theme_bw()
  # p <- plot_ly(z = xx, type = "heatmap")
  # ggplotly(p) # works great



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
  l <- layout(s, showlegend = FALSE)
  print(l)

}






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
