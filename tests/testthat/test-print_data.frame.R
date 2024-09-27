test_that("printing abbreviated data.frames works", {
  set.seed(1)
  x <- data.frame("a" = rnorm(20), "b" = rnorm(20))
  expect_snapshot(
    print_data.frame(x, rows = 20, digits = 0)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, digits = 1)
  )
  expect_snapshot(
    print_data.frame(x, rows = 7, digits = 1, row.names = FALSE)
  )
  expect_snapshot(
    print_data.frame(data.frame())
  )
  expect_snapshot(
    print_data.frame(data.frame(x = numeric()))
  )
})
