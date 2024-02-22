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
