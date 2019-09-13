#' @export
ggheatmap <- function(..., row_dend_left = FALSE) {
  plots <- heatmaply(..., row_dend_left = row_dend_left, return_ppxpy = TRUE)
  arrange_plots(
    plots, 
    row_dend_left = row_dend_left
  )
}


## TODO: duplication with heatmap_subplot_from_ggplotly
#' @importFrom grid unit.pmax 
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplotGrob
arrange_plots <- function(plots, row_dend_left = FALSE) {
  plots <- plots[!sapply(plots, is.null)]
  # if (!row_dend_left) {
  #   plots$p <- plots$p + theme(legend.position = "left")
  # }
  plots[] <- lapply(plots, ggplotGrob)

  column_list <- list(plots$py, plots$pc, plots$p)
  ind_null_col <- sapply(column_list, is.null)

  if (!is.null(plots$py)) {
    plots$py$widths <- unit.pmax(
      plots$py$widths,
      plots$p$widths,
      plots$pc$widths
    )
  }
  if (!is.null(plots$pc)) {
    plots$pc$widths <- unit.pmax(
      plots$py$widths,
      plots$p$widths,
      plots$pc$widths
    )
  }
  plots$p$widths <- unit.pmax(
    plots$py$widths,
    plots$p$widths,
    plots$pc$widths
  )

  if (!is.null(plots$px)) {
    plots$px$heights <- unit.pmax(
      plots$p$heights,
      plots$px$heights,
      plots$pr$heights
    )
  }
  if (!is.null(plots$pr)) {
    plots$pr$heights <- unit.pmax(
      plots$p$heights,
      plots$px$heights,
      plots$pr$heights
    )
  }
  plots$p$heights <- unit.pmax(
    plots$p$heights,
    plots$px$heights,
    plots$pr$heights
  )

  row1_list <- list(plots$py, ggplot_empty(), ggplot_empty())
  row2_list <- list(plots$pc, ggplot_empty(), ggplot_empty())
  row3_list <- list(plots$p, plots$pr, plots$px)


  if (row_dend_left) {
    row3_list <- rev(row3_list)
    row2_list <- rev(row2_list)
    row1_list <- rev(row1_list)
  }
  plots <- c(
    row1_list,
    row2_list,
    row3_list
  )

  nrows <- sum(!ind_null_col)
  ind_remove_col <- rep(ind_null_col, each = length(plots) / 3)

  ind_null_row <- sapply(row3_list, is.null)
  ind_remove_row <- rep(ind_null_row, length.out = length(plots))
  plots <- plots[!(ind_remove_row | ind_remove_col)]

  grid.arrange(grobs = plots, nrow = nrows)
}

ggplot_empty <- function() {
  ggplot() + theme_void()
}
