context("heatmapr")

test_that("seriate", {
  expect_is(heatmapr(mtcars, seriate = "GW"), "heatmapr")
})

test_that("cellnote", {
  expect_is(heatmapr(mtcars, seriate = "GW", cellnote = mtcars), "heatmapr")
})


test_that("Rowv, Colv", {
  hmapr <- heatmapr(mtcars, Colv = 1:ncol(mtcars), Rowv = 1:ncol(mtcars))
  expect_true(is.heatmapr(hmapr))
})

test_that("symm, revc", {
  expect_is(heatmapr(cor(mtcars), revC = TRUE), "heatmapr")
})

test_that("scale", {
  expect_is(h1 <- heatmapr(cor(mtcars), scale = "row", symm = TRUE), "heatmapr")
  expect_is(h2 <- heatmapr(cor(mtcars), scale = "col", symm = TRUE), "heatmapr")
})

test_that("show_dendrogram error conditions", {
  expect_error(heatmapr(mtcars, show_dendrogram = 1))
  expect_error(heatmapr(mtcars, show_dendrogram = rep(TRUE, 3)))
  expect_error(heatmapr(mtcars, show_dendrogram = NULL))
  expect_error(heatmapr(mtcars, show_dendrogram = TRUE), NA)
  expect_error(heatmapr(mtcars, show_dendrogram = c(TRUE, FALSE)), NA)
})


test_that("create_dend works", {
  for (ser in c("mean", "none", "OLO", "GW")) {
    expect_error(create_dend(as.matrix(mtcars), ser, dist, hclust, TRUE), NA)
    expect_error(create_dend(as.matrix(mtcars), ser, dist, hclust, FALSE), NA)
  }
})

test_that("fix_not_all_unique works", {
  expect_equal(fix_not_all_unique(1:10), 1:10)
  expect_warning(fix_not_all_unique(rep(1, 3)), "Not all")
})

test_that("k_row/col works", {
  expect_equal(attr(heatmapr(mtcars, k_col=5)$cols[[2]][[2]], "edgePar")$col, "#C29DDE")
  expect_equal(attr(heatmapr(mtcars, k_row=5)$rows[[2]][[2]][[2]][[2]], "edgePar")$col, "#C29DDE")
})
