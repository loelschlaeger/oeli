test_that("(Inverse-) Wishart density can be computed", {
  x <- diag(2)
  df <- 4
  scale <- matrix(c(1, 0.2, 0.2, 2), 2, 2)
  factor <- 1000
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = FALSE, inv = FALSE) * factor) / factor,
    0.005
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = TRUE, inv = FALSE) * factor) / factor,
    -5.335
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = FALSE, inv = TRUE) * factor) / factor,
    0.034
  )
  expect_equal(
    round(dwishart(x = x, df = df, scale = scale, log = TRUE, inv = TRUE) * factor) / factor,
    -3.378
  )
  expect_error(
    dmvnorm(dwishart(x = x, df = 1, scale = scale)),
    "Assertion on 'df' failed: Element 1 is not >= 2."
  )
})

test_that("(Inverse-) Wishart can be drawn", {
  df <- 4
  scale <- matrix(c(1, 0.2, 0.2, 2), 2, 2)
  expect_equal(dim(rwishart(df = df, scale = scale, inv = FALSE)), c(2, 2))
  expect_equal(dim(rwishart(df = df, scale = scale, inv = TRUE)), c(2, 2))
  expect_error(dmvnorm(rwishart(df = 1, scale = numeric(3))))
})
