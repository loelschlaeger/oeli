#' Silence R code
#'
#' @description
#' This function silences warnings, messages and any `cat()` or `print()`
#' output from R expressions or functions.
#'
#' @param x \[`expression`\]\cr
#' Any function or expression or value assignment expression.
#'
#' @param print_cat \[`logical(1)`\]\cr
#' Silence `print()` and `cat()` outputs?
#'
#' @param message \[`logical(1)`\]\cr
#' Silence messages?
#'
#' @param warning \[`logical(1)`\]\cr
#' Silence warnings?
#'
#' @return
#' Invisibly the expression `x`.
#'
#' @keywords functional
#' @family function helpers
#' @export
#'
#' @references
#' This function is a modified version of \code{\link[spsUtil]{quiet}}.
#'
#' @examples
#' f <- function() {
#'   warning("warning")
#'   message("message")
#'   cat("cat")
#'   print("print")
#' }
#' quiet(f())

quiet <- function(
    x, print_cat = TRUE, message = TRUE, warning = TRUE
  ) {

  ### input checks
  input_check_response(checkmate::check_flag(print_cat), "print_cat")
  input_check_response(checkmate::check_flag(message), "message")
  input_check_response(checkmate::check_flag(warning), "warning")

  ### silence code
  if (print_cat) {
    sink(tempfile(), type = "output")
    on.exit(sink())
  }
  if (warning && message) {
    invisible(force(suppressMessages(suppressWarnings(x))))
  } else if (warning && !message) {
    invisible(suppressWarnings(force(x)))
  } else if (!warning && message) {
    invisible(suppressMessages(force(x)))
  } else {
    invisible(force(x))
  }
}
