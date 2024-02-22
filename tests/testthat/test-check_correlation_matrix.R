test_that("check for correlation matrix works", {
  # Test a non-numeric matrix
  non_numeric_matrix <- matrix(letters[1:9], nrow = 3)
  expect_error(
    assert_correlation_matrix(non_numeric_matrix),
    "Must store numerics"
  )

  # Test a non-square matrix
  non_square_matrix <- matrix(1:6, nrow = 2)
  expect_error(
    assert_correlation_matrix(non_square_matrix),
    "Must be square"
  )

  # Test a non-symmetric matrix
  non_symmetric_matrix <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(
    assert_correlation_matrix(non_symmetric_matrix),
    "Must be symmetric"
  )

  # Test a matrix with bad diagonal
  matrix_with_bad_diagonal <- matrix(c(1, 0, 0, 0), nrow = 2)
  expect_error(
    assert_correlation_matrix(matrix_with_bad_diagonal),
    "Assertion on 'matrix_with_bad_diagonal' failed: Must have ones on the diagonal."
  )

  # Test a matrix with bad correlations
  matrix_with_bad_correlations <- matrix(c(1, 2, 2, 1), nrow = 2)
  expect_error(
    assert_correlation_matrix(matrix_with_bad_correlations),
    "Assertion on 'matrix_with_bad_correlations' failed: Must have values between -1 and 1."
  )

  # Test a matrix with the wrong dimensions
  matrix_with_wrong_dimensions <- diag(3)
  expect_error(
    assert_correlation_matrix(matrix_with_wrong_dimensions, dim = 2),
    "Must be of dimension 2"
  )

  # Test a matrix with the correct dimensions
  matrix_with_correct_dimensions <- diag(2)
  expect_silent(assert_correlation_matrix(matrix_with_correct_dimensions, dim = 2))

  # Test a matrix with NAs
  matrix_with_nas <- matrix(c(1, NA, NA, 1), 2, 2)
  expect_error(
    assert_correlation_matrix(matrix_with_nas),
    "Assertion on 'matrix_with_nas' failed: Must not have NA values."
  )

  # Test a matrix with infinite values
  matrix_with_inf <- matrix(c(1, Inf, Inf, 1), 2, 2)
  expect_error(
    assert_correlation_matrix(matrix_with_inf),
    "Assertion on 'matrix_with_inf' failed: Must not have infinite values."
  )
})
