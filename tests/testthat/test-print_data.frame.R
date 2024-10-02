test_that("printing abbreviated data.frames works", {
  set.seed(1)
  x <- data.frame(1:10, LETTERS[1:10], stats::rnorm(10))
  expect_snapshot(
    print_data.frame(x, rows = 7)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, cols = 2)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, cols = 2, digits = 1)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, cols = 2, digits = 1, row.names = FALSE)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, cols = 2, digits = 1, col.names = FALSE)
  )
  expect_snapshot(
    print_data.frame(data.frame())
  )
  expect_snapshot(
    print_data.frame(data.frame(x = numeric()))
  )
})
