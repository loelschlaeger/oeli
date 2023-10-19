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
})
