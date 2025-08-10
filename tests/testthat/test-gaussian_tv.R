test_that("Gaussian total variation can be computed", {

  ### identity
  m <- c(0, 0)
  S <- diag(2)
  tv <- gaussian_tv(m, m, S, S, method = "auto")
  expect_equal(tv, 0, tolerance = 1e-10)

  ### symmetry
  set.seed(1)
  m1 <- c(0, 0); m2 <- c(1, -1)
  S1 <- diag(2)
  S2 <- matrix(c(1.5, 0.3, 0.3, 1.2), 2)
  tv_a <- gaussian_tv(m1, m2, S1, S2, method = "mc", n = 1000)
  tv_b <- gaussian_tv(m2, m1, S2, S1, method = "mc", n = 1000)
  expect_equal(tv_a, tv_b, tolerance = 0.05)

  ### equal covariances
  s2 <- 1; Sig1D <- matrix(s2, 1, 1)
  for (d in c(0, 0.5, 1, 2, 3)) {
    mu1 <- 0; mu2 <- d
    tv_num <- gaussian_tv(mu1, mu2, Sig1D, Sig1D, method = "auto")
    tv_ref <- 1 - 2 * pnorm(-abs(mu1 - mu2) / (2 * sqrt(s2)))
    expect_equal(tv_num, tv_ref, tolerance = 1e-12)
  }

  ### cubature method
  mean1 <- 0
  mean2 <- 1
  Sigma1 <- 1
  Sigma2 <- 1.5
  tv_cub <- gaussian_tv(mean1, mean2, Sigma1, Sigma2, method = "cubature")
  expect_equal(tv_cub, 0.35, tolerance = 0.05)

  ### monotonicity
  ds <- c(0, 0.25, 0.5, 1, 2)
  tvs <- vapply(
    ds,
    function(d) gaussian_tv(0, d, Sig1D, Sig1D, method = "auto"), numeric(1)
  )
  expect_true(all(diff(tvs) >= -1e-10))
})
