test_that("covariance matrix can be sampled", {
  expect_silent(
    assert_covariance_matrix(sample_covariance_matrix(10))
  )
  expect_silent(
    assert_covariance_matrix(sample_covariance_matrix(3, diag = TRUE))
  )
})
