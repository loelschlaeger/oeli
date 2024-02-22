test_that("getting matrix indices works", {
  M <- diag(3)
  expect_equal(
    matrix_indices(M),
    c("11", "21", "31", "12", "22", "32", "13", "23", "33")
  )
  expect_equal(
    matrix_indices(M, "M_"),
    c("M_11", "M_21", "M_31", "M_12", "M_22", "M_32", "M_13", "M_23", "M_33")
  )
  expect_equal(
    matrix_indices(M, "M_", TRUE),
    c("M_21", "M_31", "M_12", "M_32", "M_13", "M_23")
  )
})
