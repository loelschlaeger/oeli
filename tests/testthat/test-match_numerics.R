test_that("brute force matching works", {
  expect_equal(
    match_numerics(1:9, 9:1),
    9:1
  )
})

test_that("matching minimizes the sum of deviations", {
  expect_identical(match_numerics(c(0, 3), c(2, 5)), c(1L, 2L))
  expect_identical(match_numerics(c(100, 101), c(101.4, 100.6)), c(2L, 1L))
  expect_identical(
    match_numerics(c(-1, 0, 1), c(0.1, 1.5, -1.2)), c(2L, 3L, 1L)
  )
  expect_error(match_numerics(c(1, NA), c(1, 2)), "Input `x` is bad")
})
