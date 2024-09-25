test_that("Cholesky decomposition works", {
  cov <- sample_covariance_matrix(5)
  chol <- cov_to_chol(cov)
  expect_true(all.equal(cov, chol_to_cov(chol)))
  chol <- cov_to_chol(cov)
  expect_true(all.equal(cov, chol_to_cov(chol)))
  expect_equal(unique_chol(numeric()), numeric())
  expect_equal(cov_to_chol(matrix(0, 2, 2)), c(0, 0, 0))
})
