test_that("can group data.frame", {
  df <- data.frame("label" = c("A", "B"), "number" = 1:10)
  expect_equal(
    group_data_frame(df = df, by = "label"),
    list(
      A = structure(
        list(label = c("A", "A", "A", "A", "A"), number = c(1L, 3L, 5L, 7L, 9L)),
        row.names = c(1L, 3L, 5L, 7L, 9L), class = "data.frame"
      ),
      B = structure(
        list(label = c("B", "B", "B", "B", "B"), number = c(2L, 4L, 6L, 8L, 10L)),
        row.names = c(2L, 4L, 6L, 8L, 10L), class = "data.frame"
      )
    )
  )
  expect_equal(
    group_data_frame(df = df, by = "label", keep_by = FALSE),
    list(
      A = structure(
        list(number = c(1L, 3L, 5L, 7L, 9L)),
        row.names = c(1L, 3L, 5L, 7L, 9L),
        class = "data.frame"
      ),
      B = structure(
        list(number = c(2L, 4L, 6L, 8L, 10L)),
        row.names = c(2L, 4L, 6L, 8L, 10L),
        class = "data.frame"
      )
    )
  )
})

