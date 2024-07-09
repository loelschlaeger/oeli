test_that("default values of a function can be returned", {
  f <- function(a, b = 1, c = "", ...) { }
  expect_equal(
    function_defaults(f),
    list(b = 1, c = "")
  )
  expect_equal(
    function_defaults(f, exclude = "b"),
    list(c = "")
  )
})
