test_that("stationary distribution can be computed", {
  tpm <- matrix(0.05, nrow = 3, ncol = 3)
  diag(tpm) <- 0.9
  expect_equal(
    stationary_distribution(tpm),
    c(1, 1, 1) / 3
  )
})

test_that("soft fail works", {
  tpm <- matrix(c(
    1, 0, 0,
    0.5, 0.5, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)
  expect_equal(
    stationary_distribution(tpm, soft_fail = TRUE),
    c(1, 1, 1) / 3
  )
  expect_error(
    stationary_distribution(tpm),
    "singular system"
  )
})
