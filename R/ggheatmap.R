#
# # source: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
# # http://rstudio-pubs-static.s3.amazonaws.com/2852_379274d7c5734f979e106dcf019ec46c.html
# multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
#   # library(grid)
#
#   plots <- c(list(...), plotlist)
#
#   numPlots = length(plots)
#
#   if (is.null(layout)) {
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#
#   if (numPlots == 1) {
#     print(plots[[1]])
#
#   } else {
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
#     for (i in 1:numPlots) {
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

# grid,
# gridExtra

#' @title  Creates a ggplot2 heatmap
#'
#' @description
#' An object of class heatmapr includes all the needed information
#' for producing a heatmap. The goal is to seperate the pre-processing of the
#' heatmap elements from the graphical rendaring of the object, which could be done
#'
#' @param x can either be a heatmapr object, or a numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#'
#' @param ... other parameters passed to \link{heatmapr} (currently, various parameters may be ignored.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' library(heatmaply)
#' x <- heatmapr(iris[,-5], scale = "column", colors = "Blues")
#' ggheatmap(x)
#'
#'
#' }
ggheatmap <- function(x, ...) {
  if(!is.heatmapr(x)) {
    x <- heatmapr(x, ...)
  }
  ppxpy <- heatmaply(x, return_ppxpy = TRUE)
  ggempty <- ggplot() + geom_blank() + theme_bw()
  gg_list <- list(ppxpy$py, ggempty, ppxpy$p, ppxpy$px)
  # gg_list <- list(ppxpy$py, ppxpy$p, ggempty, ppxpy$px)
  # sapply(ppxpy, class)
  # lapply(gg_list, class)
  GGally::ggmatrix(gg_list, nrow = 2, ncol = 2) # in this we can't control the relative widths
  # multiplot(ppxpy$py, ggempty, ppxpy$p, ppxpy$px,

  # pp <- ppxpy$p + guides(fill=FALSE) +
  #   theme(axis.title.x=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank(),
  #         axis.title.y=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.ticks.y=element_blank())
  # multiplot(ppxpy$py, pp , ggempty, ppxpy$px,
  #           layout = matrix(c(1,2,2,1,2,2,3,4,4), nrow = 3) ,
  #                     cols = 2, main = "Main title")

  # ppxpy$p + guides(fill=FALSE)

}


