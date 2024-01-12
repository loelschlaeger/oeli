test_that("plotting a sequence works", {
  expect_s3_class(
    plot_sequence(
      definition = function(n) ifelse(n %in% factorial(1:10), 1, 1 / n),
      start = 1, end = 9, limits = c(0, 1)
    ),
    "ggplot"
  )
})
