context("plots")

iris_plot <- iris[, -5]
iris_category <- iris[, 5, drop = FALSE]

test_that("ggplot_heatmap works", {
	g <- heatmaply:::ggplot_heatmap(as.matrix(iris_plot))
	expect_is(g, "ggplot")
    g <- heatmaply:::ggplot_heatmap(as.matrix(iris_plot),
        label_names=c("a", "b", "c"))
    expect_is(g, "ggplot")
    expect_error(heatmaply:::ggplot_heatmap(as.matrix(iris_plot),
        label_names=c("a")))
})

test_that("plotly_heatmap works", {
	p <- heatmaply:::plotly_heatmap(as.matrix(iris_plot))
	expect_is(p, "plotly")
})

test_that("plotly_dend works", {
	hc <- hclust(dist(iris_plot))
	p <- heatmaply:::plotly_dend(hc)
	expect_is(p, "plotly")
})

test_that("ggplot_side_color_plot works", {
	g <- heatmaply:::ggplot_side_color_plot(iris_category,
		type = "row")
	expect_is(g, "ggplot")
	expect_error(ggplotly(g), NA)
	g <- heatmaply:::ggplot_side_color_plot(iris_category,
		type = "column")
	expect_is(g, "ggplot")
	expect_error(ggplotly(g), NA)
})

test_that("plotly_side_color_plot works", {
	p <- heatmaply:::plotly_side_color_plot(iris_category,
		type = "row")
	expect_is(p, "plotly")
	expect_error(ggplotly(p), NA)
	p <- heatmaply:::plotly_side_color_plot(iris_category,
		type = "column")
	expect_is(p, "plotly")
	expect_error(ggplotly(p), NA)
})

test_that("predict_colors works", {
	expect_error(heatmaply:::predict_colors("#ffffff"))
	p <- heatmaply:::plotly_heatmap(as.matrix(iris_plot))
	g <- heatmaply:::ggplot_heatmap(as.matrix(iris_plot))
	expect_error(
        heatmaply:::predict_colors(p, plot_method = "plotly"),
        NA)
	expect_error(
		heatmaply:::predict_colors(ggplotly(g), plot_method = "ggplot"),
		NA)
})

test_that("parse_plotly_color works", {
	expect_equal(
		heatmaply:::parse_plotly_color("rgb(1,1,1,0)"),
		heatmaply:::parse_plotly_color("rgb(1,1,1)"),
		"#010101")
	expect_equal(
		toupper(heatmaply:::parse_plotly_color("rgb(255,255,255)")),
		"#FFFFFF"
	)
	expect_equal(
		heatmaply:::parse_plotly_color("rgb(0,0,0)"),
		"#000000"
	)

})

test_that("k_colors works", {
	expect_equal(heatmaply:::k_colors(1), "black")
	expect_equal(heatmaply:::k_colors(5), colorspace::rainbow_hcl(5))
})

test_that("default colors works", {
    for (i in 1:50) {
        expect_equal(length(heatmaply:::default_side_colors(i)), i)
    }
})

test_that("numeric cols on dendrograms", {
    dend <- as.dendrogram(hclust(dist(mtcars)))
    dend <- dendextend::set(dend, "branches_k_color", 1:32)
    expect_silent(heatmaply:::plotly_dend(dend))
})

test_that("side colors fail when bad palette", {
    expect_error(
        heatmaply:::ggplot_side_color_plot(iris[, 5, drop=FALSE]),
        NA)
    expect_error(
        heatmaply:::ggplot_side_color_plot(iris[, 5, drop=FALSE],
            palette = setNames(RColorBrewer::brewer.pal(3, "Set3"),
                unique(iris[, 5]))),
        NA)
    expect_error(
        heatmaply:::ggplot_side_color_plot(iris[, 5, drop=FALSE],
            palette = c("Nope" = "Nope")))

    expect_error(
        heatmaply:::plotly_side_color_plot(iris[, 5, drop=FALSE]),
        NA)
    expect_error(
        heatmaply:::plotly_side_color_plot(iris[, 5, drop=FALSE],
            palette = setNames(RColorBrewer::brewer.pal(3, "Set3"),
                unique(iris[, 5]))),
        NA)
    expect_error(
        heatmaply:::plotly_side_color_plot(iris[, 5, drop=FALSE],
            palette = c("Nope" = "Nope")))
})

test_that("plotly dend uses colors", {
    dend <- hclust(dist(mtcars))
    dend <- dendextend::color_branches(dend, k=2)
    p <- heatmaply:::plotly_dend(dend)
    expect_is(p, "plotly")
    dend <- dendextend::color_branches(dend, k=2, col=c(1, 2))
    p <- heatmaply:::plotly_dend(dend)
    expect_is(p, "plotly")
})
