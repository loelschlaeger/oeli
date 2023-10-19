test_that("stationary distribution can be computed", {
  tpm <- matrix(0.05, nrow = 3, ncol = 3)
  diag(tpm) <- 0.9
  expect_equal(
    stationary_distribution(tpm),
    c(1, 1, 1) / 3
  )
})
