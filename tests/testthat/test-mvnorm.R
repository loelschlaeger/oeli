test_that("Univariate normal density can be computed", {
  x <- 1
  mean <- 2
  Sigma <- 4
  expect_equal(dmvnorm(x, mean, Sigma), dnorm(x, mean, sqrt(Sigma)))
  expect_error(
    dmvnorm(x = 1:2, mean = 1:3, Sigma = diag(4)),
    "Input `mean` is bad: Must have length 2, but has length 3"
  )
})

test_that("Multivariate normal density can be computed", {
  x <- c(0, 0)
  mean <- c(0, 0)
  Sigma <- diag(2)
  factor <- 1000
  expect_equal(
    round(dmvnorm(x = x, mean = mean, Sigma = Sigma, log = FALSE) * factor) / factor,
    0.159
  )
  expect_equal(
    round(dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE) * factor) / factor,
    -1.838
  )
  expect_error(
    dmvnorm(x = 1:2, mean = 1:3, Sigma = diag(4)),
    "Input `mean` is bad: Must have length 2, but has length 3"
  )
})

test_that("Univariate normal can be drawn", {
  mean <- 0
  Sigma <- 1
  expect_length(rmvnorm(mean = mean, Sigma = Sigma, log = FALSE), 1)
  expect_length(rmvnorm(mean = mean, Sigma = Sigma, log = TRUE), 1)
  checkmate::expect_matrix(
    rmvnorm(n = 5, mean = mean, Sigma = Sigma),
    ncols = 1, nrows = 5
  )
})

test_that("Multivariate normal can be drawn", {
  mean <- c(0, 0)
  Sigma <- diag(2)
  expect_length(rmvnorm(mean = mean, Sigma = Sigma, log = FALSE), 2)
  expect_length(rmvnorm(mean = mean, Sigma = Sigma, log = TRUE), 2)
  checkmate::expect_matrix(
    rmvnorm(n = 5, mean = mean, Sigma = Sigma),
    ncols = 2, nrows = 5
  )
  expect_error(rmvnorm(mean = 1:3, Sigma = diag(4)))
})
