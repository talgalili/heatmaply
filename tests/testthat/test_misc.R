context("misc")

testthat("is.na10", {
    tf <- rep(c(TRUE, FALSE), length.out = 10)
    expect_equal(is.na10(tf), tf)
})
