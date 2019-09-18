#' ggplot heatmap equivalent to heatmaply
#' 
#' This function produces a ggplot analogue of heatmaply figures 
#' using \link[egg]{ggarrange}. This function may not always support the same
#' set of features as , and exporting the heatmaply object with, for example,
#' \link[plotly]{orca} or \code{heatmaply(mtcars, file = "foo.png")}.
#' 
#' @param ... Passed to \link{heatmaply}
#' @param widths,heights Relative widths and heights of plots.
#' @param row_dend_left Logical argument controlling whether the row 
#'  dendrogram is placed on the left of the plot.
#' @examples
#' ggheatmap(mtcars)
#' @export
ggheatmap <- function(..., widths = NULL, heights = NULL, row_dend_left = FALSE) {
  plots <- heatmaply(
    ..., 
    row_dend_left = row_dend_left, 
    return_ppxpy = TRUE,
    plot_method = "ggplot"
  )
  arrange_plots(
    plots, 
    widths = widths,
    heights = heights,
    row_dend_left = row_dend_left
  )
}


## TODO: duplication with heatmap_subplot_from_ggplotly
arrange_plots <- function(
    plots, 
    widths = NULL, 
    heights = NULL, 
    row_dend_left = FALSE) {

  plots <- plots[!sapply(plots, is.null)]
  if (!row_dend_left) {
    plots$p <- plots$p + theme(legend.position = "left")
  }
  plots <- lapply(plots, function(x) x + theme(plot.margin = unit(c(0, 0, 0, 0), "npc")))

  column_list <- list(plots$py, plots$pc, plots$p)
  ind_null_col <- sapply(column_list, is.null)

  row1_list <- list(plots$py, ggplot_empty(), ggplot_empty())
  row2_list <- list(plots$pc, ggplot_empty(), ggplot_empty())
  row3_list <- list(plots$p, plots$pr, plots$px)

  if (row_dend_left) {
    row3_list <- rev(row3_list)
    row2_list <- rev(row2_list)
    row1_list <- rev(row1_list)
  }
  plotlist <- c(
    row1_list,
    row2_list,
    row3_list
  )

  nrows <- sum(!ind_null_col)
  ind_remove_col <- rep(ind_null_col, each = length(plotlist) / 3)

  ind_null_row <- sapply(row3_list, is.null)
  ncols <- sum(!ind_null_row)
  ind_remove_row <- rep(ind_null_row, length.out = length(plotlist))
  plotlist <- plotlist[!(ind_remove_row | ind_remove_col)]

  egg::ggarrange(
    plots = plotlist,
    ncol = ncols,
    widths = widths %||% default_dims(plots$px, plots$pr),
    heights = heights %||% rev(default_dims(plots$py, plots$pc))
  )
}

ggplot_empty <- function() {
  ggplot() + theme_void() + theme(plot.margin = unit(c(0, 0, 0, 0), "npc"))
}
