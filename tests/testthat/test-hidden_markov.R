test_that("log-likelihood computation for HMM works", {
  states <- 2
  sdd <- "normal"
  x <- simulate_hmm(
    Tp = 100, states = states, sdd = sdd, mean = c(-1, 1), sd = c(1, 1)
  )
  theta <- as.vector(x$parameter)
  checkmate::expect_number(
    ll_hmm(theta = theta, data = x$data, states = states, sdd = sdd)
  )
})

test_that("simulation of HMM data works", {
  x <- simulate_hmm(
    Tp = 100, states = 2, sdd = "normal", mean = c(-1, 1), sd = c(1, 1)
  )
  checkmate::expect_list(x)
})
