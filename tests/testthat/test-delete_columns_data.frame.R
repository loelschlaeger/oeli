test_that("can delete data.frame columns", {
  df <- data.frame("label" = c("A", "B"), "number" = 1:10)
  expect_equal(
    delete_columns_data.frame(df = df, column_names = "label"),
    structure(list(number = 1:10), row.names = c(NA, -10L), class = "data.frame")
  )
  expect_equal(
    delete_columns_data.frame(df = df, column_names = "number"),
    structure(
      list(label = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B")),
      row.names = c(NA, -10L), class = "data.frame"
    )
  )
  expect_equal(
    delete_columns_data.frame(df = df, column_names = c("label", "number")),
    structure(list(), names = character(0), row.names = c(NA, -10L), class = "data.frame")
  )
})
