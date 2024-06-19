test_that("can get indices of matrix diagonal", {
  expect_identical(matrix_diagonal_indices(3), c(1L, 5L, 9L))
  expect_identical(matrix_diagonal_indices(3, triangular = "lower"), c(1L, 4L, 6L))
  expect_identical(matrix_diagonal_indices(3, triangular = "upper"), c(1L, 3L, 6L))
})
