test_that("Confirmation by user works", {
  f <- file()
  options("oeli_connection" = f)
  ans <- paste(c("n", "y", "", "bad", "y"), collapse = "\n")
  write(ans, f)
  suppressMessages({
    expect_false(user_confirm())
    expect_true(user_confirm())
    expect_false(user_confirm())
    expect_true(user_confirm())
  })
  options("oeli_connection" = stdin())
  close(f)
  if (!interactive()) {
    expect_false(user_confirm())
  }
})
