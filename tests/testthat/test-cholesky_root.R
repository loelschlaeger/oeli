test_that("Cholesky decomposition works", {
  cov <- sample_covariance_matrix(4)
  chol <- cov_2_chol(cov)
  expect_true(all.equal(cov, chol_2_cov(chol)))
  chol <- cov_2_chol(cov, unique = FALSE)
  expect_true(all.equal(cov, chol_2_cov(chol)))
})
