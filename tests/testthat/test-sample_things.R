test_that("Dirichlet can be drawn", {
  concentration <- 1:3
  expect_length(rdirichlet(concentration = concentration), 3)
  checkmate::expect_matrix(
    rdirichlet(n = 5, concentration = concentration),
    ncols = 3, nrows = 5
  )
  expect_error(
    rdirichlet(concentration = diag(3)),
    "Assertion on 'concentration' failed: Must be of type 'vector', not 'matrix'."
  )
  checkmate::expect_matrix(
    rdirichlet(n = 5, concentration = 1),
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

test_that("(Inverse-) Wishart can be drawn", {
  df <- 4
  scale <- matrix(c(1, 0.2, 0.2, 2), 2, 2)
  expect_equal(dim(rwishart(df = df, scale = scale, inv = FALSE)), c(2, 2))
  expect_equal(dim(rwishart(df = df, scale = scale, inv = TRUE)), c(2, 2))
  expect_error(dmvnorm(rwishart(df = 1, scale = numeric(3))))
})

test_that("transition probabiliy matrix can be sampled", {
  expect_true(
    check_transition_probability_matrix(
      sample_transition_probability_matrix(3)
    )
  )
})

test_that("covariance matrix can be sampled", {
  expect_true(
    check_covariance_matrix(sample_covariance_matrix(3))
  )
  expect_true(
    check_covariance_matrix(sample_covariance_matrix(3, diag = TRUE))
  )
})
