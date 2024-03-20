test_that("insert vector entry works", {
  v <- 1:3
  x <- 0
  expect_equal(
    insert_vector_entry(v, x, 0),
    c(0, 1, 2, 3)
  )
  expect_equal(
    insert_vector_entry(v, x, 1),
    c(1, 0, 2, 3)
  )
  expect_equal(
    insert_vector_entry(v, x, 2),
    c(1, 2, 0, 3)
  )
  expect_equal(
    insert_vector_entry(v, x, 3),
    c(1, 2, 3, 0)
  )
  expect_equal(
    insert_vector_entry(v, x, 0:3),
    c(0, 1, 0, 2, 0, 3, 0)
  )
  expect_equal(
    insert_vector_entry(integer(), integer(), integer()),
    integer()
  )
})
