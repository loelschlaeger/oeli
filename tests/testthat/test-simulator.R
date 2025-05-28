<<<<<<< HEAD
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
=======
#
# ### define progress bar and parallel computation
# progressr::handlers(global = TRUE)
# progressr::handlers(
#   progressr::handler_progress(format = ">> :percent, :eta to go :message")
# )
# # future::plan(future::multisession, workers = 2)
#
# f <- function() { Sys.sleep(runif(1)); rnorm(1, sd = 0.1) }
#
# g <- function(x, y) { Sys.sleep(runif(1)); x + y + rnorm(1, sd = 0.1) }
#
# h <- function(x, y = 1) { Sys.sleep(runif(1)); x + y + rnorm(1, sd = 0.1) }
#
#
#
# sim <- Simulator$new(verbose = TRUE)
#
# self <- sim
# private <- self$.__enclos_env__$private
#
# sim$define(f = g, x = as.list(1:2), y = as.list(1:2))
#
# sim$go(runs = 2)
#
# sim$results
>>>>>>> f82728beed0c515939faaf1ce9d22e8098f0af17
