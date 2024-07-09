test_that("determination of variable name works", {
  expect_equal(
    variable_name(a),
    "a"
  )
  f <- function(x) variable_name(x)
  expect_equal(
    f(a),
    "a"
  )
  expect_equal(
    variable_name(function(x) {
      mean(x)
    }),
    "unnamed"
  )
})
