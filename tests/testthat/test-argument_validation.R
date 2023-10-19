test_that("Argument matching works", {
  expect_equal(
    match_arg("A", "A"),
    "A"
  )
  expect_equal(
    match_arg("A", LETTERS),
    "A"
  )
  expect_error(
    match_arg(c("A", "B"), LETTERS),
    "must be of length 1"
  )
  expect_equal(
    match_arg("lo", c("loooong", "else")),
    "loooong"
  )
  expect_equal(
    match_arg(c("A", "B"), LETTERS, several.ok = TRUE),
    c("A", "B")
  )
  expect_error(
    match_arg(character(), LETTERS),
    "greater or equal 1"
  )
  expect_equal(
    match_arg(character(), LETTERS, none.ok = TRUE),
    character()
  )
  expect_error(
    match_arg("bad", LETTERS),
    "must be one of"
  )
})

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
})

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
})

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
})

test_that("check for probability vector works", {
  # Test a non-numeric vector
  non_numeric_vector <- letters[1:9]
  expect_error(
    assert_probability_vector(non_numeric_vector),
    "Must be of type 'numeric', not 'character'."
  )

  # Test a vector with bad elements
  vector_with_bad_elements <- 0:2
  expect_error(
    assert_probability_vector(vector_with_bad_elements),
    "Assertion on 'vector_with_bad_elements' failed: Element 3 is not <= 1."
  )

  # Test a vector with bad sum
  vector_with_bad_sum <- c(1, 1:5 / sum(1:5))
  expect_error(
    assert_probability_vector(vector_with_bad_sum),
    "Assertion on 'vector_with_bad_sum' failed: Must add up to 1."
  )

  # Test a vector with the wrong dimensions
  vector_with_wrong_length <- 1:5 / sum(1:5)
  expect_error(
    assert_probability_vector(vector_with_wrong_length, len = 4),
    "Assertion on 'vector_with_wrong_length' failed: Must have length 4, but has length 5."
  )

  # Test a vector with the correct dimensions
  vector_with_correct_length <- 1:5 / sum(1:5)
  expect_silent(assert_probability_vector(vector_with_correct_length, len = 5))
})
