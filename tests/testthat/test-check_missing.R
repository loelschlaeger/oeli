test_that("check for missing formal argument works", {

  f <- function(x, type) {
    switch(
      type,
      "check" = check_missing(x),
      "test" = test_missing(x),
      "assert" = assert_missing(x)
    )
  }

  expect_equal(f(type = "check"), "Argument needs a value")
  expect_true(f(1, type = "check"))

  expect_false(f(type = "test"))
  expect_true(f(1, type = "test"))

  expect_error(f(type = "assert"), "Assertion on 'x' failed: Argument needs a value.")
  expect_silent(f(1, type = "assert"))
})
