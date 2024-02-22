test_that("check for transition probability matrix works", {
  # Test a non-numeric matrix
  non_numeric_matrix <- matrix(letters[1:9], nrow = 3)
  expect_error(
    assert_transition_probability_matrix(non_numeric_matrix),
    "Must store numerics"
  )

  # Test a non-square matrix
  non_square_matrix <- matrix(1:6, nrow = 2)
  expect_error(
    assert_transition_probability_matrix(non_square_matrix),
    "Must be square"
  )

  # Test a matrix with bad rows
  matrix_with_bad_rows <- matrix(c(1, 2, 2, 1), nrow = 2)
  expect_error(
    assert_transition_probability_matrix(matrix_with_bad_rows),
    "Assertion on 'matrix_with_bad_rows' failed: Must have values between 0 and 1."
  )

  # Test a matrix with bad row sum
  matrix_with_bad_row_sums <- matrix(c(1, 1, 0, 1), nrow = 2)
  expect_error(
    assert_transition_probability_matrix(matrix_with_bad_row_sums),
    "Assertion on 'matrix_with_bad_row_sums' failed: Must have row sums equal to 1."
  )

  # Test a matrix with the wrong dimensions
  matrix_with_wrong_dimensions <- diag(3)
  expect_error(
    assert_transition_probability_matrix(matrix_with_wrong_dimensions, dim = 2),
    "Must be of dimension 2"
  )

  # Test a matrix with the correct dimensions
  matrix_with_correct_dimensions <- diag(2)
  expect_silent(assert_transition_probability_matrix(matrix_with_correct_dimensions, dim = 2))

  # Deal with machine epsilon
  matrix_rounding_issues <- structure(
    c(0.96413163700029, 0.342824884561103, 0.0358683629997102, 0.657175115438897),
    dim = c(2L, 2L)
  )
  expect_false(
    test_transition_probability_matrix(matrix_rounding_issues, tolerance = 0)
  )
  expect_true(
    test_transition_probability_matrix(matrix_rounding_issues)
  )

  # Test a matrix with NAs
  matrix_with_nas <- matrix(c(0.5, NA, NA, 0.5), 2, 2)
  expect_error(
    assert_transition_probability_matrix(matrix_with_nas),
    "Assertion on 'matrix_with_nas' failed: Must not have NA values."
  )

  # Test a matrix with infinite values
  matrix_with_inf <- matrix(c(0.5, Inf, Inf, 0.5), 2, 2)
  expect_error(
    assert_transition_probability_matrix(matrix_with_inf),
    "Assertion on 'matrix_with_inf' failed: Must not have infinite values."
  )
})
