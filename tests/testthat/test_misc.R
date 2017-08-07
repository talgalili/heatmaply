context("misc")

test_that("is.na10", {
  tf <- rep(c(NA, FALSE), length.out = 10)
  expect_equal(is.na10(tf), as.integer(is.na(tf)))
})

test_that("normalize", {
  a <- seq(0,1, by=0.01)
  expect_equal(normalize(a), a)
  b <- 1:1000000
  expect_true(all(normalize(b) <= 1 && all(normalize(b) >= 0)))

  d <- data.frame(a=a[1:101], b=b[1:101], c="c")
  dn <- normalize(d)
  expect_equal(normalize(d[, 1:2]), normalize(as.matrix(d[, 1:2])))
  expect_equal(dn[, 1], a)
  expect_true(all(dn[[3]] == "c"))
})




test_that("percentize", {
  a <- seq(0,1, by=0.01)
  expect_equal(percentize(a), unname(unlist(percentize(data.frame(a)))))
})




