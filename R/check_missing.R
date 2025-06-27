#' Check missing formal argument
#'
#' @description
#' These functions check whether a value was specified as an argument to a
#' function.
#'
#' @param x \[`any`\]\cr
#' A formal argument.
#'
#' @return
#' Depending on the function prefix:
#' - If the check is successful, `assert_missing()` returns `x` invisibly,
#' whereas `check_missing()` and `test_missing()` return `TRUE`.
#' - If the check is not successful, `assert_missing()` throws an error message,
#' `test_missing()` returns `FALSE`, and `check_missing()` returns a string with
#' the error message.
#'
#' @keywords validation
#' @family package helpers
#' @export
#'
#' @examples
#' f <- function(x) {
#'   check_missing(x)
#' }
#' f()
#'
#' g <- function(x) {
#'   test_missing(x)
#' }
#' g()
#'
#' h <- function(x) {
#'   assert_missing(x)
#' }
#' \dontrun{
#' h()
#' }

check_missing <- function(x) {
  x_missing <- missing(x)
  if (isTRUE(x_missing)) {
    return("Argument needs a value")
  }
  return(TRUE)
}

#' @rdname check_missing
#' @export

assert_missing <- function(x) {
  checkmate::makeAssertion(
    x,
    check_missing(x),
    checkmate::vname(x),
    NULL
  )
}

#' @rdname check_missing
#' @export

test_missing <- checkmate::makeTestFunction(
  check_missing
)
