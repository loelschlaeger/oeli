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

test_that("insert matrix column at multiple position works", {
  A <- diag(3)
  x <- 1:3
  expect_equal(
    insert_matrix_column(A, x, 0:3),
    structure(
      c(1, 2, 3, 1, 0, 0, 1, 2, 3, 0, 1, 0, 1, 2, 3, 0, 0, 1, 1, 2, 3),
      dim = c(3L, 7L)
    )
  )
})

test_that("insert single value as matrix column works", {
  A <- diag(3)
  x <- 2
  expect_equal(
    insert_matrix_column(A, x, 0),
    structure(c(2, 2, 2, 1, 0, 0, 0, 1, 0, 0, 0, 1), dim = 3:4)
  )
})

test_that("insert matrix column trivial case works", {
  expect_equal(
    insert_matrix_column(matrix(nrow = 0, ncol = 0), integer(), integer()),
    structure(logical(0), dim = c(0L, 0L))
  )
})
