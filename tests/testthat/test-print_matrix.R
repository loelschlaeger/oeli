test_that("matrix printing works", {
  expect_snapshot(
    print_matrix(x = 1)
  )
  expect_snapshot(
    print_matrix(x = 1.5, label = "single numeric")
  )
  expect_snapshot(
    print_matrix(x = 1.5, label = "single numeric", simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26])
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters")
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters", details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], label = "letters", simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], rowdots = 26)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 25)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 26)
  )
  expect_snapshot(
    print_matrix(x = LETTERS[1:26], coldots = 10)
  )
  expect_error(
    print_matrix(x = LETTERS[1:26], coldots = 0)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6))
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), rowdots = 2)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), coldots = 1, rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(LETTERS[1:24], ncol = 6), simplify = TRUE, coldots = 2, rowdots = 2)
  )
  expect_snapshot(
    print_matrix(x = diag(5), coldots = 2, rowdots = 3, simplify = TRUE, digits = 0)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1))
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, nrow = 1), simplify = TRUE, coldots = 5)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1))
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), label = "single column matrix")
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), details = FALSE)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), coldots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), rowdots = 1)
  )
  expect_snapshot(
    print_matrix(x = matrix(1:100, ncol = 1), simplify = TRUE, coldots = 5)
  )
  expect_snapshot(
    print_matrix(x = diag(2), simplify = TRUE)
  )
  expect_snapshot(
    print_matrix(x = diag(2), simplify = FALSE)
  )
  expect_snapshot({
    print_matrix(structure(diag(3), dimnames = list(c("la", "le", "lu"), c("x", "y", "z"))))
  })
})
