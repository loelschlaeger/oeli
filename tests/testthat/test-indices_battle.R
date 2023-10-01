test_that("Removing index works", {
  expect_equal(remove_index(1:10, 1), 2:10)
  expect_equal(remove_index(1:10, 1, replace = TRUE), 1:9)
})
