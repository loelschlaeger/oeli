test_that("splitting a vector works", {
  expect_equal(
    split_vector_at(1:10, 1:10),
    as.list(1:10)
  )
  expect_equal(
    split_vector_at(1:10, c(2, 3, 5, 7)),
    list(1L, 2L, 3:4, 5:6, 7:10)
  )
  expect_equal(
    split_vector_at(1:10, numeric()),
    list(1:10)
  )
  expect_error(
    split_vector_at(1:10, 0),
    "Input `at` is bad: Element 1 is not >= 1"
  )
  expect_error(
    split_vector_at(1:10, 11),
    "Input `at` is bad: Element 1 is not <= 10"
  )
  expect_equal(
    split_vector_at(numeric(), numeric()),
    list(numeric())
  )
  expect_equal(
    split_vector_at(1, 1),
    list(1)
  )
})
