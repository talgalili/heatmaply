context("heatmapr")

test_that("seriate", {
    expect_is(heatmapr(mtcars, seriate = "GW"), "heatmapr")
})


test_that("Rowv, Colv", {
    expect_is(heatmapr(mtcars, Colv = 1:ncol(mtcars), Rowv = 1:ncol(mtcars)), 
        "heatmapr")
})

test_that("symm, revc", {
    expect_is(heatmapr(cor(mtcars), revC = TRUE), "heatmapr")
})

test_that("scale", {
    expect_is(h1 <- heatmapr(cor(mtcars), scale = "row", symm=TRUE), "heatmapr")
    expect_is(h2 <- heatmapr(cor(mtcars), scale = "col", symm=TRUE), "heatmapr")
})
