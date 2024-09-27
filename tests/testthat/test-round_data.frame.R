test_that("rounding numeric data.frame columns works", {
  set.seed(1)
  df <- data.frame("label" = c("A", "B"), "number" = rnorm(10) * 10)
  expect_equal(
    round_data.frame(df, NULL), df
  )
  expect_equal(
    round_data.frame(df),
    structure(
      list(
        label = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"),
        number = c(-6, 2, -8, 16, 3, -8, 5, 7, 6, -3)
      ),
      row.names = c(NA, -10L),
      class = "data.frame"
    )
  )
})
