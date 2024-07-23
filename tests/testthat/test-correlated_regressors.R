test_that("simulating regressors works", {

  ### multivariate normal regressors
  labels <- c("N1", "N2", "N3")
  n <- 100
  data <- correlated_regressors(labels, n, verbose = FALSE)
  checkmate::expect_data_frame(data, col.names = "strict", nrows = n)

  ### custom marginals
  set.seed(1)
  labels <- c("P", "C", "N1", "N2", "U")
  n <- 100
  marginals <- list(
    "P" = list(type = "poisson", lambda = 1),
    "C" = list(type = "categorical", p = c(0.2, 0.3, 0.5)),
    "N1" = list(type = "normal", mean = -1, sd = 2),
    "U" = list(type = "uniform", min = -2, max = -1)
  )
  correlation <- matrix(
    c(1, 0.44, -0.67, 0.23, 0.17,
      0.44, 1, -0.59, 0.17, -0.21,
      -0.67, -0.59, 1, -0.18, -0.13,
      0.23, 0.17, -0.18, 1, -0.56,
      0.17, -0.21, -0.13, -0.56, 1),
    nrow = 5, ncol = 5
  )
  data <- correlated_regressors(
    labels = labels, n = n, marginals = marginals, correlation = correlation,
    verbose = FALSE
  )
  checkmate::expect_data_frame(data, col.names = "strict", nrows = n)
})
