test_that("generating subsets works", {
  v <- 1:3
  expect_equal(
    subsets(v),
    list(1L, 2L, 3L, 1:2, c(1L, 3L), 2:3, 1:3)
  )
  expect_equal(
    subsets(v, n = c(1, 3)),
    list(1L, 2L, 3L, 1:3)
  )
  expect_equal(
    subsets(v = integer()),
    list()
  )
  expect_equal(
    subsets(v, n = integer()),
    list()
  )
  expect_equal(
    subsets(LETTERS[1:3]),
    list("A", "B", "C", c("A", "B"), c("A", "C"), c("B", "C"), c("A", "B", "C"))
  )
})
