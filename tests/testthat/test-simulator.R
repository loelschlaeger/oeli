test_that("Simulator case 1 works", {
  f <- function() { Sys.sleep(runif(1)); rnorm(1, sd = 0.1) }
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = f)
  sim$go(runs = 2, backup = FALSE)
  checkmate::expect_r6(sim, "Simulator")
})

test_that("Simulator case 1 works", {
  g <- function(x, y) { Sys.sleep(runif(1)); x + y + rnorm(1, sd = 0.1) }
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = g, x = as.list(1:2), y = as.list(2:3))
  sim$go(runs = 2, backup = FALSE)
  checkmate::expect_r6(sim, "Simulator")
})

test_that("Simulator case 3 works", {
  h <- function(x, y = 1) { Sys.sleep(runif(1)); x + y + rnorm(1, sd = 0.1) }
  sim <- Simulator$new(verbose = FALSE)
  sim$define(f = h, x = as.list(1:2))
  sim$go(runs = 2, backup = FALSE)
  checkmate::expect_r6(sim, "Simulator")
})
