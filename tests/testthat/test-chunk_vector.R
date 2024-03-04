test_that("chunking a vector works", {
  checkmate::expect_list(
    chunk_vector(LETTERS, n = 4), len = 4
  )
  expect_equal(
    chunk_vector(1:12, n = 3, type = 1),
    list("1" = 1:4, "2" = 5:8, "3" = 9:12)
  )
  expect_equal(
    chunk_vector(1:12, n = 3, type = 2),
    list("1" = 1:3, "2" = 4:6, "3" = 7:9, "4" = 10:12)
  )
  expect_error(
    chunk_vector(1:10, n = 4, strict = TRUE),
    "is not a multiple"
  )
})
