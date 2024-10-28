test_that("mapping indices works", {
  expect_equal(
    map_indices(c(1, 3, 5), 0),
    integer()
  )
  expect_equal(
    map_indices(numeric(), 3),
    integer()
  )
  expect_equal(
    map_indices(c(1, 3, 5), 1),
    c(1L, 3L, 5L)
  )
  expect_equal(
    map_indices(c(1, 3, 5), 3),
    c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L)
  )
})
