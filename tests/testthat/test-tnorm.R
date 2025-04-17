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
  density_a_log_rounded <- round(density_a_log * factor) / factor
  expect_equal(density_a_log_rounded, 0)
})

test_that("univariate truncated normal can be drawn", {
  mean <- 0.0
  sd <- 1.0
  point <- 0.0

  draw_tb <- rtnorm(mean, sd, point, FALSE, FALSE)
  expect_true(draw_tb > 0.0)

  draw_ta <- rtnorm(mean, sd, point, TRUE, FALSE)
  expect_true(draw_ta < 0.0)
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
  density_b_log_rounded <- round(density_b_log * factor) / factor
  expect_equal(density_b_log_rounded, 0)

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
  density_a_log_rounded <- round(density_a_log * factor) / factor
  expect_equal(density_a_log_rounded, 0)
})

test_that("univariate doubly truncated normal can be drawn", {
  mean <- 0.0
  sd <- 1.0
  lower <- -1.0
  upper <- 1.0

  draw <- rttnorm(mean, sd, lower, upper)
  expect_true(draw > -1.0)
  expect_true(draw < 1.0)
})
