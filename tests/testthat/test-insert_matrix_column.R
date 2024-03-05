test_that("insert matrix column works", {
  A <- diag(3)
  x <- numeric(3)
  expect_equal(
    insert_matrix_column(A, x, 0),
    structure(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), dim = 3:4)
  )
  expect_equal(
    insert_matrix_column(A, x, 1),
    structure(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1), dim = 3:4)
  )
  expect_equal(
    insert_matrix_column(A, x, 2),
    structure(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1), dim = 3:4)
  )
  expect_equal(
    insert_matrix_column(A, x, 3),
    structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), dim = 3:4)
  )
})
