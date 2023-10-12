test_that("differencing works", {
  expect_error(
    delta(0, 3),
    "Assertion on 'ref' failed: Element 1 is not >= 1."
  )
  expect_equal(
    delta(1, 3),
    structure(c(-1, -1, 1, 0, 0, 1), dim = 2:3)
  )
  expect_equal(
    delta(2, 3),
    structure(c(1, 0, -1, -1, 0, 1), dim = 2:3)
  )
  expect_equal(
    delta(3, 3),
    structure(c(1, 0, 0, 1, -1, -1), dim = 2:3)
  )
  expect_error(
    delta(4, 3),
    "Assertion on 'ref' failed: Element 1 is not <= 3."
  )
  n <- sample(2:10, 1)
  ref <- sample.int(n, 1)
  Sigma0 <- sample_covariance_matrix(dim = n)
  expect_silent(assert_covariance_matrix(Sigma0))
  Sigma_diff0 <- diff_cov(Sigma0, ref = ref)
  expect_silent(assert_covariance_matrix(Sigma_diff0))
  Sigma1 <- undiff_cov(Sigma_diff0, ref = ref)
  expect_silent(assert_covariance_matrix(Sigma1))
  Sigma_diff1 <- diff_cov(Sigma1, ref = ref)
  expect_silent(assert_covariance_matrix(Sigma_diff1))
  expect_equal(Sigma_diff0, Sigma_diff1)
})
