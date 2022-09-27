for (plot_method in c("ggplot", "plotly")) {
  for (bool in c(TRUE, FALSE)) {
    context(paste0(plot_method, ", row_dend_left=", bool))

    test_that("heatmaply mtcars (both dend)", {
      h <- heatmaply(
        mtcars,
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (no dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "none", row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (coldend only)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "col", row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rowdend only)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "row", row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })


    test_that("heatmaply mtcars (rscols, both dend)", {
      h <- heatmaply(
        mtcars,
        row_side_colors = mtcars[, 1:2], row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })


    test_that("heatmaply mtcars (rscols, row dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "none",
        row_side_colors = mtcars[, 1:2],
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rscols, row dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "col",
        row_side_colors = mtcars[, 1:2],
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rscols, no dends)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "none",
        row_side_colors = mtcars[, 1:2], row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (cscols, both dend)", {
      h <- heatmaply(
        mtcars,
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (cscols, col dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "col",
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (cscols, row dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "row",
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (cscols, no dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "none",
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rcscols, both dend)", {
      h <- heatmaply(
        mtcars,
        row_side_colors = mtcars[, 1:2],
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rcscols, col dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "col",
        row_side_colors = mtcars[, 1:2],
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rcscols, row dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "row",
        row_side_colors = mtcars[, 1:2],
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })

    test_that("heatmaply mtcars (rcscols, no dend)", {
      h <- heatmaply(
        mtcars,
        dendrogram = "none",
        row_side_colors = mtcars[, 1:2],
        col_side_colors = data.frame(t(mtcars[1:2, ])),
        row_dend_left = bool,
        plot_method = plot_method
      )
      expect_is(h, "plotly")
    })
  }
}
