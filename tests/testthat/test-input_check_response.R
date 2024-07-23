test_that("response to input check works", {
  x <- "1"
  y <- 1
  expect_true(
    input_check_response(
      check = checkmate::check_character(x),
      var_name = "x",
      error = TRUE
    )
  )
  expect_true(
    input_check_response(
      check = checkmate::check_character(x),
      var_name = "x",
      error = FALSE
    )
  )
  expect_false(
    input_check_response(
      check = checkmate::check_character(y),
      var_name = "y",
      error = FALSE
    )
  )
  expect_error(
    input_check_response(
      check = checkmate::check_character(y),
      var_name = "y",
      error = TRUE
    ),
    "Input `y` is bad: Must be of type 'character', not 'double'"
  )
})
