test_that("check for list of lists works", {
  # Test an empty list
  empty_list <- list()
  expect_true(test_list_of_lists(empty_list))
  expect_error(
    assert_list_of_lists(empty_list, len = 1),
    "Assertion on 'empty_list' failed: Must have length 1, but has length 0."
  )

  # Test a list with non-list elements
  non_list_elements <- list(1, 2)
  expect_error(
    assert_list_of_lists(non_list_elements),
    "Assertion on 'non_list_elements' failed: Check for element 1 failed: Must be of type 'list', not 'double'."
  )

  # Test a list with only list elements
  only_list_elements <- list(list(), list(1))
  expect_true(test_list_of_lists(only_list_elements))
})
