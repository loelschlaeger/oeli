test_that("correlation matrix can be sampled", {
  expect_true(
    check_correlation_matrix(sample_correlation_matrix(3))
  )
})
