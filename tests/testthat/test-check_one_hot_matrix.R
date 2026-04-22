test_that("check for one-hot matrix works", {
  # Test a non-numeric matrix
  non_numeric_matrix <- matrix(letters[1:9], nrow = 3)
  expect_error(
    assert_one_hot_matrix(non_numeric_matrix),
    "Must store numerics"
  )

  # Test a matrix with missing values
  matrix_with_nas <- matrix(c(1, 0, NA, 1), nrow = 2)
  expect_error(
    assert_one_hot_matrix(matrix_with_nas),
    "Assertion on 'matrix_with_nas' failed: Must not contain missing values."
  )

  # Test a matrix with infinite values
  matrix_with_inf <- matrix(c(1, 0, Inf, 1), nrow = 2)
  expect_error(
    assert_one_hot_matrix(matrix_with_inf),
    "Assertion on 'matrix_with_inf' failed: Must not contain infinite values."
  )

  # Test a matrix with values different from 0 and 1
  matrix_with_bad_values <- matrix(c(1, 0, 0.2, 0.8), nrow = 2)
  expect_error(
    assert_one_hot_matrix(matrix_with_bad_values, tolerance = 0),
    "Assertion on 'matrix_with_bad_values' failed: All elements must be 0 or 1."
  )

  # Test a matrix with no 1 in a row
  matrix_with_no_one <- matrix(c(1, 0, 0, 0), nrow = 2)
  expect_error(
    assert_one_hot_matrix(matrix_with_no_one),
    "Assertion on 'matrix_with_no_one' failed: Each row must contain exactly one 1."
  )

  # Test a matrix with more than one 1 in a row
  matrix_with_multiple_ones <- matrix(c(1, 0, 1, 1), nrow = 2)
  expect_error(
    assert_one_hot_matrix(matrix_with_multiple_ones),
    "Assertion on 'matrix_with_multiple_ones' failed: Each row must contain exactly one 1."
  )

  # Test a matrix with the wrong number of rows
  matrix_with_wrong_nrow <- diag(2)
  expect_error(
    assert_one_hot_matrix(matrix_with_wrong_nrow, nrows = 3),
    "Must have exactly 3 rows, but has 2 rows."
  )

  # Test a matrix with the wrong number of columns
  matrix_with_wrong_ncol <- diag(2)
  expect_error(
    assert_one_hot_matrix(matrix_with_wrong_ncol, ncols = 3),
    "Must have exactly 3 cols, but has 2 cols."
  )

  # Deal with machine epsilon
  matrix_rounding_issues <- structure(
    c(1, 0, sqrt(.Machine$double.eps) / 2, 1 - sqrt(.Machine$double.eps) / 2),
    dim = c(2L, 2L)
  )
  expect_false(
    test_one_hot_matrix(matrix_rounding_issues, tolerance = 0)
  )
  expect_true(
    test_one_hot_matrix(matrix_rounding_issues)
  )

  # Test a valid one-hot matrix
  valid_one_hot_matrix <- matrix(c(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)
  expect_silent(
    assert_one_hot_matrix(valid_one_hot_matrix)
  )
  expect_true(
    test_one_hot_matrix(valid_one_hot_matrix)
  )
})
