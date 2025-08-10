test_that("Mixture normal density can be computed", {

  ### univariate
  x <- 1
  mean <- matrix(c(-1, 0, 1), ncol = 3)
  Sigma <- matrix(c(0.5, 1, 1.5), ncol = 3)
  proportions <- c(0.5, 0.3, 0.2)
  p <- dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_equal(
    p,
    proportions[1] * dnorm(x, mean[1], sqrt(Sigma[, 1])) +
      proportions[2] * dnorm(x, mean[2], sqrt(Sigma[, 2])) +
      proportions[3] * dnorm(x, mean[3], sqrt(Sigma[, 3]))
  )
  expect_equal(
    p,
    dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions * 2)
  )

  ### multivariate
  x <- c(0, 0)
  mean <- matrix(c(-1, -1, 0, 0), ncol = 2)
  Sigma <- matrix(c(diag(2), diag(2)), ncol = 2)
  proportions <- c(0.7, 0.3)
  factor <- 1000
  expect_equal(
    round(dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions) *
            factor) / factor,
    0.089
  )

  ### one component
  x <- c(0, 0)
  mean <- matrix(c(-1, -1), ncol = 1)
  Sigma <- matrix(c(diag(2)), ncol = 1)
  proportions <- 1
  expect_equal(
    dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions),
    dmvnorm(x = x, mean = as.vector(mean), Sigma = matrix(Sigma, nrow = 2, ncol = 2))
  )
})

test_that("Mixture normal CDF can be computed", {

  ### univariate
  x <- 1
  mean <- matrix(c(-1, 0, 1), ncol = 3)
  Sigma <- matrix(c(0.5, 1, 1.5), ncol = 3)
  proportions <- c(0.5, 0.3, 0.2)
  p <- pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_equal(
    p,
    proportions[1] * pnorm(x, mean[1], sqrt(Sigma[, 1])) +
      proportions[2] * pnorm(x, mean[2], sqrt(Sigma[, 2])) +
      proportions[3] * pnorm(x, mean[3], sqrt(Sigma[, 3]))
  )
  expect_equal(
    p,
    pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions * 2)
  )

  ### multivariate
  x <- c(0, 0)
  mean <- matrix(c(-1, -1, 0, 0), ncol = 2)
  Sigma <- matrix(c(diag(2), diag(2)), ncol = 2)
  proportions <- c(0.7, 0.3)
  factor <- 1000
  expect_equal(
    round(pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions) *
            factor) / factor,
    0.571
  )

  ### one component
  x <- c(0, 0)
  mean <- matrix(c(-1, -1), ncol = 1)
  Sigma <- matrix(c(diag(2)), ncol = 1)
  proportions <- 1
  expect_equal(
    pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions),
    as.numeric(pmvnorm(
      x = x,
      mean = as.vector(mean),
      Sigma = matrix(Sigma, nrow = 2, ncol = 2)
    ))
  )
})

test_that("Mixture normal draws can be generated", {

  set.seed(1)

  ### univariate case
  mean <- matrix(0, nrow = 1, ncol = 1)
  Sigma <- matrix(1, ncol = 1)
  proportions <- 1
  x1 <- rmixnorm(n = 1, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_true(is.numeric(x1) && length(x1) == 1 && !is.matrix(x1))
  x5 <- rmixnorm(n = 5, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_true(is.matrix(x5))
  expect_equal(dim(x5), c(5L, 1L))
  mean <- matrix(c(-1, 0, 1), ncol = 3)
  Sigma <- matrix(c(0.5, 1, 1.5), ncol = 3)
  proportions <- c(0.5, 0.3, 0.2)
  w <- proportions / sum(proportions)
  mu <- as.numeric(mean)
  vars <- as.numeric(Sigma)
  m_theo <- sum(w * mu)
  v_theo <- sum(w * (vars + mu^2)) - m_theo^2
  n <- 10000
  samp <- rmixnorm(n = n, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_true(is.matrix(samp))
  expect_equal(dim(samp), c(n, 1))
  m_emp <- mean(samp)
  v_emp <- var(drop(samp))
  expect_equal(m_emp, m_theo, tolerance = 0.1)
  expect_equal(as.numeric(v_emp), v_theo, tolerance = 0.1)
  samp2 <- rmixnorm(n = n, mean = mean, Sigma = Sigma, proportions = proportions * 2)
  m_emp2 <- mean(samp2)
  v_emp2 <- var(drop(samp2))
  expect_lt(abs(m_emp - m_emp2), 0.1)
  expect_lt(abs(as.numeric(v_emp) - as.numeric(v_emp2)), 0.1)

  ### bivariate case
  mean2 <- matrix(c(0, 0), nrow = 2, ncol = 1)
  Sigma2 <- matrix(c(diag(2)), ncol = 1)
  y1 <- rmixnorm(n = 1, mean = mean2, Sigma = Sigma2, proportions = 1)
  expect_true(is.numeric(y1) && length(y1) == 2 && !is.matrix(y1))
  y7 <- rmixnorm(n = 7, mean = mean2, Sigma = Sigma2, proportions = 1)
  expect_true(is.matrix(y7))
  expect_equal(dim(y7), c(7L, 2L))
  mean <- matrix(c(-1, -1, 0, 0), ncol = 2)
  Sigma <- matrix(c(diag(2), diag(2)), ncol = 2)
  proportions <- c(0.7, 0.3)
  w <- proportions / sum(proportions)
  m_theo <- drop(mean %*% w)
  dimx <- nrow(mean)
  comps <- ncol(mean)
  S_acc <- matrix(0, nrow = dimx, ncol = dimx)
  for (k in seq_len(comps)) {
    mu_k <- matrix(mean[, k], ncol = 1)
    Sig_k <- matrix(Sigma[, k], nrow = dimx, ncol = dimx)
    S_acc <- S_acc + w[k] * (Sig_k + mu_k %*% t(mu_k))
  }
  S_theo <- S_acc - m_theo %*% t(m_theo)
  n <- 10000
  samp <- rmixnorm(n = n, mean = mean, Sigma = Sigma, proportions = proportions)
  expect_true(is.matrix(samp))
  expect_equal(dim(samp), c(n, dimx))
  m_emp <- colMeans(samp)
  S_emp <- stats::cov(samp)
  expect_equal(m_emp, as.numeric(m_theo), tolerance = 0.1)
  expect_equal(S_emp, S_theo, tolerance = 0.1)
  samp2 <- rmixnorm(n = n, mean = mean, Sigma = Sigma, proportions = proportions * 10)
  m_emp2 <- colMeans(samp2)
  expect_lt(max(abs(m_emp - m_emp2)), 0.1)
})
