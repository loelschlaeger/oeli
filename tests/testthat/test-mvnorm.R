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
    round(dmvnorm(x = x, mean = mean[1], Sigma = Sigma, log = FALSE) * factor) / factor,
    0.159
  )
  expect_equal(
    round(dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE) * factor) / factor,
    -1.838
  )
  expect_error(
    dmvnorm(x = 1:2, mean = 1:3, Sigma = diag(2)),
    "Input `mean` is bad: Must have length 2, but has length 3"
  )
})

test_that("Univariate normal CDF can be computed", {
  x <- 1
  mean <- 2
  Sigma <- 4
  expect_equal(pmvnorm(x, mean, Sigma), pnorm(x, mean, sqrt(Sigma)))
  expect_error(
    pmvnorm(x = 1:2, mean = 1:3, Sigma = diag(4)),
    "Input `mean` is bad: Must have length 2, but has length 3"
  )
})

test_that("Multivariate normal CDF can be computed", {
  x <- c(-0.78, -1.17)
  mean <- c(0.64, 0.97)
  Sigma <- matrix(c(3.35, 1.45, 1.45, 3.03), ncol = 2)
  factor <- 1000
  expect_equal(
    round(pmvnorm(x = x, mean = mean, Sigma = Sigma) * factor) / factor,
    0.055
  )
  expect_error(
    pmvnorm(x = 1:2, mean = 1:3, Sigma = diag(4)),
    "Input `mean` is bad: Must have length 2, but has length 3"
  )
})

test_that("Multivariate normal rectangle probability can be computed", {
  Sigma <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
  factor <- 1000
  expect_equal(
    round(pmvnorm(x = c(0, 0), mean = 0, Sigma = Sigma) * factor) / factor,
    0.333
  )
  expect_equal(
    pmvnorm(x = c(1, 1), mean = 0, Sigma = diag(2), lower = -1),
    (pnorm(1) - pnorm(-1))^2
  )
  Sigma3 <- matrix(0.5, 3, 3) + diag(0.5, 3)
  expect_equal(pmvnorm(x = rep(0, 3), mean = 0, Sigma = Sigma3), 0.25)
  Sigma4 <- matrix(0.5, 4, 4) + diag(0.5, 4)
  expect_equal(
    pmvnorm(x = rep(0, 4), mean = 0, Sigma = Sigma4, method = "ghk"), 0.2,
    tolerance = 0.01
  )
  expect_identical(
    pmvnorm(x = rep(0, 4), mean = 0, Sigma = Sigma4, method = "ghk"),
    pmvnorm(x = rep(0, 4), mean = 0, Sigma = Sigma4, method = "ghk")
  )
  expect_equal(pmvnorm(x = c(0, 0), mean = 0, Sigma = diag(2), lower = 0), 0)
  expect_error(
    pmvnorm(x = c(0, 0), mean = 0, Sigma = diag(2), method = "exact"),
    "Input `method` is bad"
  )
  expect_error(
    pmvnorm(x = c(0, 0), mean = 0, Sigma = diag(2), lower = 1:3),
    "Input `lower` is bad"
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

test_that("Multivariate normal with zero dimensions can be drawn", {
  dim <- 4
  zero_dim <- c(2, 3)
  mean <- round(rnorm(dim), 2)
  Sigma <- sample_covariance_matrix(dim = dim)
  Sigma[zero_dim, ] <- Sigma[, zero_dim] <- 0

  ### n = 1
  sample <- rmvnorm(n = 1, mean = mean, Sigma = Sigma, log = FALSE)
  expect_equal(sample[zero_dim], mean[zero_dim])
  sample <- rmvnorm(n = 1, mean = mean, Sigma = Sigma, log = TRUE)
  expect_equal(sample[zero_dim], exp(mean[zero_dim]))

  ### n > 1
  n <- 3
  sample <- rmvnorm(n = n, mean = mean, Sigma = Sigma, log = FALSE)
  expect_equal(
    sample[, zero_dim],
    matrix(mean[zero_dim], nrow = n, ncol = length(zero_dim), byrow = TRUE)
  )
  sample <- rmvnorm(n = n, mean = mean, Sigma = Sigma, log = TRUE)
  expect_equal(
    sample[, zero_dim],
    exp(matrix(mean[zero_dim], nrow = n, ncol = length(zero_dim), byrow = TRUE))
  )

  ### dim = 1
  mean <- round(rnorm(1), 2)
  expect_equal(
    rmvnorm(n = 1, mean = mean, Sigma = 0, log = FALSE),
    mean
  )
  expect_equal(
    rmvnorm(n = 1, mean = mean, Sigma = 0, log = TRUE),
    exp(mean)
  )
  n <- 3
  expect_equal(
    rmvnorm(n = n, mean = mean, Sigma = 0, log = FALSE),
    matrix(mean, nrow = n)
  )
  expect_equal(
    rmvnorm(n = n, mean = mean, Sigma = 0, log = TRUE),
    matrix(exp(mean), nrow = n)
  )
})
