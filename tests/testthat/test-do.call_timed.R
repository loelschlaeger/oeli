test_that("measure computation time works", {
  what <- function(s) {
    Sys.sleep(s)
    return(s)
  }
  args <- list(s = 1)
  out <- do.call_timed(what = what, args = args)
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(out[["result"]], 1)
  expect_type(out[["time"]], "double")
  expect_true(abs(out[["time"]] - 2) < 1)
})
