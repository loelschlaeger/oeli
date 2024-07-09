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
