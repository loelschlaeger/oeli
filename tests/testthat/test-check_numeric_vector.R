test_that("check for numeric vector works", {
  # Test a non-numeric vector
  non_numeric_vector <- letters[1:9]
  expect_error(
    assert_numeric_vector(non_numeric_vector),
    "Must be of type 'numeric', not 'character'."
  )

  # Test a vector with bad elements
  vector_with_bad_elements <- 0:2
  expect_error(
    assert_numeric_vector(vector_with_bad_elements, lower = 1),
    "Assertion on 'vector_with_bad_elements' failed: Element 1 is not >= 1."
  )

  # Test a vector with bad class
  vector_with_bad_class <- diag(2)
  expect_error(
    assert_numeric_vector(vector_with_bad_class),
    "Assertion on 'vector_with_bad_class' failed: Must be of type 'atomic vector', not 'matrix'."
  )

  # Test NULL
  expect_true(
    test_numeric_vector(NULL, null.ok = TRUE)
  )
  expect_false(
    test_numeric_vector(NULL, null.ok = FALSE)
  )
})
