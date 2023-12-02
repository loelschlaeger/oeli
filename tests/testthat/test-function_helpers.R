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

test_that("extraction of function body as character works", {
  test_fun <- function(x) {
    stopifnot(is.numeric(x))
    {
      x + 1
    }
  }
  expect_equal(
    function_body(test_fun),
    "stopifnot(is.numeric(x)) { x + 1 }"
  )
  expect_equal(
    function_body(test_fun, braces = TRUE),
    "{ stopifnot(is.numeric(x)) { x + 1 } }"
  )
  expect_equal(
    function_body(test_fun, nchar = 20),
    "stopifnot(is.nume..."
  )
})

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
