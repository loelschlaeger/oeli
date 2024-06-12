test_that("finding occurrence in vector works", {
  x <- c("A", "A", "B", "C", "A")
  expect_equal(vector_occurrence(x, "first"), c(1L, 3L, 4L))
  expect_equal(vector_occurrence(x, "last"), c(5L, 3L, 4L))
})
