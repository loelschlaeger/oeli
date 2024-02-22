test_that("check for covariance matrix works", {
  # Test a non-numeric matrix
  non_numeric_matrix <- matrix(letters[1:9], nrow = 3)
  expect_error(
    assert_covariance_matrix(non_numeric_matrix),
    "Must store numerics"
  )

  # Test a non-square matrix
  non_square_matrix <- matrix(1:6, nrow = 2)
  expect_error(
    assert_covariance_matrix(non_square_matrix),
    "Must be square"
  )

  # Test a non-symmetric matrix
  non_symmetric_matrix <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(
    assert_covariance_matrix(non_symmetric_matrix),
    "Must be symmetric"
  )

  # Test a matrix with negative eigenvalues
  matrix_with_negative_eigenvalues <- matrix(c(1, 2, 2, 1), nrow = 2) * -1
  expect_error(
    assert_covariance_matrix(matrix_with_negative_eigenvalues),
    "Must have positive eigenvalues only"
  )

  # Test a matrix with the wrong dimensions
  matrix_with_wrong_dimensions <- diag(3)
  expect_error(
    assert_covariance_matrix(matrix_with_wrong_dimensions, dim = 2),
    "Must be of dimension 2"
  )

  # Test a matrix with the correct dimensions
  matrix_with_correct_dimensions <- diag(2)
  expect_silent(assert_covariance_matrix(matrix_with_correct_dimensions, dim = 2))

  # Test a matrix with NAs
  matrix_with_nas <- matrix(c(1, NA, NA, 2), 2, 2)
  expect_error(
    assert_covariance_matrix(matrix_with_nas),
    "Assertion on 'matrix_with_nas' failed: Must not have NA values."
  )

  # Test a matrix with infinite values
  matrix_with_inf <- matrix(c(1, Inf, Inf, 2), 2, 2)
  expect_error(
    assert_covariance_matrix(matrix_with_inf),
    "Assertion on 'matrix_with_inf' failed: Must not have infinite values."
  )
})
