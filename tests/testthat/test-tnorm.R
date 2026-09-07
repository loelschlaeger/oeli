test_that("univariate truncated normal density can be computed", {
  x <- 1.0
  mean <- 0.0
  sd <- 1.0
  point <- 0.0
  factor <- 1000

  density_b <- dtnorm(x, mean, sd, point, FALSE, FALSE)
  density_b_rounded <- round(density_b * factor) / factor
  expect_equal(density_b_rounded, 0.484)

  density_b_log <- dtnorm(x, mean, sd, point, FALSE, TRUE)
  density_b_log_rounded <- round(density_b_log * factor) / factor
  expect_equal(density_b_log_rounded, -0.726)

  density_a <- dtnorm(x, mean, sd, point, TRUE, FALSE)
  density_a_rounded <- round(density_a * factor) / factor
  expect_equal(density_a_rounded, 0)

  density_a_log <- dtnorm(x, mean, sd, point, TRUE, TRUE)
  expect_identical(density_a_log, -Inf)
  expect_equal(
    dtnorm(10, mean, sd, 9, FALSE, TRUE),
    dnorm(10, log = TRUE) - pnorm(9, lower.tail = FALSE, log.p = TRUE)
  )
})

test_that("univariate truncated normal can be drawn", {
  mean <- 0.0
  sd <- 1.0
  point <- 0.0

  draw_tb <- rtnorm(mean = mean, sd = sd, point = point, above = FALSE)
  expect_true(draw_tb > 0.0)

  draw_ta <- rtnorm(mean = mean, sd = sd, point = point, above = TRUE)
  expect_true(draw_ta < 0.0)

  draws <- rtnorm(n = 5, mean = mean, sd = sd, point = 9, above = FALSE)
  expect_length(draws, 5)
  draw_log <- rtnorm(mean = mean, sd = sd, point = 0, above = FALSE, log = TRUE)
  expect_true(draw_log > 1)
})

test_that("univariate doubly truncated normal density can be computed", {
  x_b <- -2.0
  x_m <- 0.0
  x_a <- 2.0
  mean <- 0.0
  sd <- 1.0
  lower <- -1.0
  upper <- 1.0
  factor <- 1000

  density_b <- dttnorm(x_b, mean, sd, lower, upper, FALSE)
  density_b_rounded <- round(density_b * factor) / factor
  expect_equal(density_b_rounded, 0)

  density_b_log <- dttnorm(x_b, mean, sd, lower, upper, TRUE)
  expect_identical(density_b_log, -Inf)

  density_m <- dttnorm(x_m, mean, sd, lower, upper, FALSE)
  density_m_rounded <- round(density_m * factor) / factor
  expect_equal(density_m_rounded, 0.584)

  density_m_log <- dttnorm(x_m, mean, sd, lower, upper, TRUE)
  density_m_log_rounded <- round(density_m_log * factor) / factor
  expect_equal(density_m_log_rounded, -0.537)

  density_a <- dttnorm(x_a, mean, sd, lower, upper, FALSE)
  density_a_rounded <- round(density_a * factor) / factor
  expect_equal(density_a_rounded, 0)

  density_a_log <- dttnorm(x_a, mean, sd, lower, upper, TRUE)
  expect_identical(density_a_log, -Inf)
  expect_equal(
    dttnorm(20.5, mean, sd, 20, 21, TRUE),
    dnorm(20.5, log = TRUE) - log(pnorm(-20) - pnorm(-21))
  )
  expect_equal(dttnorm(0.5, mean, sd, -Inf, Inf), dnorm(0.5))
})

test_that("univariate doubly truncated normal can be drawn", {
  mean <- 0.0
  sd <- 1.0
  lower <- -1.0
  upper <- 1.0

  draw <- rttnorm(mean = mean, sd = sd, lower = lower, upper = upper)
  expect_true(draw > -1.0)
  expect_true(draw < 1.0)

  draws <- rttnorm(n = 4, mean = mean, sd = sd, lower = 39, upper = 40)
  expect_length(draws, 4)
  expect_true(all(draws > 39 & draws < 40))
})
