test_that("names of function arguments can be extracted", {
  f <- function(a, b = 1, c = "", ...) { }
  expect_equal(
    function_arguments(f),
    c("a", "b", "c", "...")
  )
  expect_equal(
    function_arguments(f, with_default = FALSE),
    c("a", "...")
  )
  expect_equal(
    function_arguments(f, with_ellipsis = FALSE),
    c("a", "b", "c")
  )
  f <- function() { }
  expect_equal(
    function_arguments(f),
    character()
  )
  expect_equal(
    function_arguments(f, with_default = FALSE),
    character()
  )
  expect_equal(
    function_arguments(f, with_ellipsis = FALSE),
    character()
  )
  f <- function(x, a = 20, b = 0.2, c = 2 * pi) {
    -a * exp(-b * sqrt(mean(x^2))) - exp(mean(cos(c * x))) +
      a + exp(1)
  }
  expect_equal(
    function_arguments(f, with_default = TRUE, with_ellipsis = TRUE),
    c("x", "a", "b", "c")
  )
  expect_equal(
    function_arguments(f, with_default = TRUE, with_ellipsis = FALSE),
    c("x", "a", "b", "c")
  )
  expect_equal(
    function_arguments(f, with_default = FALSE, with_ellipsis = TRUE),
    c("x")
  )
  expect_equal(
    function_arguments(f, with_default = FALSE, with_ellipsis = FALSE),
    c("x")
  )
})


