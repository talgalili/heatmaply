for (plot_method in c("ggplot", "plotly")) {
    for (bool in c(TRUE, FALSE)) {

        context(paste0(plot_method, ", row_dend_left=", bool))

        test_that("heatmaply mtcars (both dend)", {
            h <- heatmaply(mtcars, row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")

        })

        test_that("heatmaply mtcars (no dend)", {
            h <- heatmaply(mtcars, dendrogram = "none", row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (coldend only)", {
            h <- heatmaply(mtcars, dendrogram = "col", row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rowdend only)", {
            h <- heatmaply(mtcars, dendrogram = "row", row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })


        test_that("heatmaply mtcars (rscols, both dend)", {
            h <- heatmaply(mtcars,
                row_side_colors = mtcars[, 1:2], row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })


        test_that("heatmaply mtcars (rscols, row dend)", {
            h <- heatmaply(mtcars, dendrogram = "none", 
                row_side_colors = mtcars[, 1:2], 
                row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rscols, row dend)", {
            h <- heatmaply(mtcars, dendrogram = "col", 
                row_side_colors = mtcars[, 1:2], 
                row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rscols, no dends)", {
            h <- heatmaply(mtcars, dendrogram = "none", 
                row_side_colors = mtcars[, 1:2], row_dend_left = bool,
                plot_method = plot_method)
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (cscols, both dend)", {
            expect_warning(h <- heatmaply(mtcars, 
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (cscols, col dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "col",
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (cscols, row dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "row",
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (cscols, no dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "none",
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rcscols, both dend)", {
            expect_warning(h <- heatmaply(mtcars,
                row_side_colors = mtcars[, 1:2],
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rcscols, col dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "col",
                row_side_colors = mtcars[, 1:2],
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rcscols, row dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "row",
                row_side_colors = mtcars[, 1:2],
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })

        test_that("heatmaply mtcars (rcscols, no dend)", {
            expect_warning(h <- heatmaply(mtcars, dendrogram = "none",
                row_side_colors = mtcars[, 1:2],
                col_side_colors = data.frame(t(mtcars[1:2, ])),
                row_dend_left = bool,
                plot_method = plot_method))
            expect_is(h, "plotly")
        })
    }
}


context("heatmaply misc")


test_that("non-numerics moved to row_side_colors", {
    mtcars[, ncol(mtcars) + 1] <- "a"
    h <- heatmaply(mtcars, row_side_colors = mtcars[, 1:2])
    expect_is(h, "plotly")
    h <- heatmaply(mtcars)
    expect_is(h, "plotly")
})


test_that("heatmaply on matrix, and cexRow/Col", {
    mtcars <- as.matrix(mtcars)
    rownames(mtcars) <- NULL
    h <- heatmaply(mtcars, cexRow = 5, cexCol = 5)
    expect_warning(heatmaply(mtcars, cexRow = "b", cexCol = "a"))

    expect_is(h, "plotly")
})


test_that("grid_color and hide_colorbar", {
    h <- heatmaply(mtcars, grid_color = "black", hide_colorbar = TRUE)
    expect_is(h, "plotly")
})

test_that("return plots", {
    h <- heatmaply(mtcars, return_ppxpy = TRUE)
    sapply(h, function(el) expect_true(is.null(el) | inherits(el, "gg")))
})

test_that("SideColors", {
    rs <- mtcars[, 1]
    rscolors <- colorRampPalette(c("red", "blue"))(length(unique(rs)))
    rs <- rscolors[as.character(rs)]
    h <- heatmaply(mtcars, RowSideColors = rs)
    expect_is(h, "plotly")
    expect_warning(h <- heatmaply(t(mtcars), ColSideColors = rs))
    expect_is(h, "plotly")
})

test_that("limits", {
    h <- heatmaply(t(mtcars), limits = range(as.matrix(mtcars)))
    expect_is(h, "plotly")
    expect_warning(h <- heatmaply(t(mtcars), limits = c(0, 0)))
    expect_is(h, "plotly")
    expect_error(heatmaply(t(mtcars), limits = c("a", "b")))
    expect_error(heatmaply(t(mtcars), limits = 1))
})

