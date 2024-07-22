test_that("silencing R code works", {
  f <- function() {
    warning("warning")
    message("message")
    cat("cat")
    print("print")
  }
  quiet({
    expect_warning(
      quiet(f(), TRUE, FALSE, FALSE),
      "warning"
    )
    quiet(f(), TRUE, FALSE, TRUE)
    expect_warning(
      quiet(f(), TRUE, TRUE, FALSE),
      "warning"
    )
    quiet(f(), TRUE, TRUE, TRUE)
    expect_warning(
      quiet(f(), FALSE, FALSE, FALSE),
      "warning"
    )
    quiet(f(), FALSE, FALSE, TRUE)
    expect_warning(
      quiet(f(), FALSE, TRUE, FALSE),
      "warning"
    )
    quiet(f(), FALSE, TRUE, TRUE)
  })
})
