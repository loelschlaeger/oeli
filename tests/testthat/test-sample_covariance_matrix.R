test_that("covariance matrix can be sampled", {
  expect_true(
    check_covariance_matrix(sample_covariance_matrix(3))
  )
  expect_true(
    check_covariance_matrix(sample_covariance_matrix(3, diag = TRUE))
  )
})
