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
