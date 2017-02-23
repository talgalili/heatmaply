context("misc")

test_that("is.na10", {
    tf <- rep(c(NA, FALSE), length.out = 10)
    expect_equal(is.na10(tf), as.integer(is.na(tf)))
})
