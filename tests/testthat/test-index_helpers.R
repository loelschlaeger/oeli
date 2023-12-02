test_that("computation of permutations works", {
  expect_equal(
    permutations(1:3),
    list(1:3, c(1L, 3L, 2L), c(2L, 1L, 3L), c(2L, 3L, 1L), c(3L, 1L, 2L), 3:1)
  )
  expect_equal(
    permutations(LETTERS[1:3]),
    list(
      c("A", "B", "C"), c("A", "C", "B"), c("B", "A", "C"), c("B", "C", "A"),
      c("C", "A", "B"), c("C", "B", "A")
    )
  )
})

test_that("brute force matching works", {
  expect_equal(
    match_numerics(1:9, 9:1),
    9:1
  )
})
