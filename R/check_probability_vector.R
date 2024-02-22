#' Check if an argument is a probability vector
#'
#' @description
#' This function checks whether the input is a real vector with non-negative
#' entries that add up to one.
#'
#' @param x
#' Object to check.
#'
#' @param tolerance
#' A non-negative \code{numeric} tolerance value.
#'
#' @inheritParams checkmate::check_numeric
#'
#' @return
#' Compare to \code{\link[checkmate]{check_numeric}}.
#'
#' @export

check_probability_vector <- function(
    x, len = NULL, tolerance = sqrt(.Machine$double.eps)
) {
  checkmate::assert_number(tolerance, lower = 0)
  res <- check_numeric_vector(
    x, any.missing = FALSE, len = len, lower = 0, upper = 1
  )
  if (!isTRUE(res)) {
    return(res)
  }
  if (abs(sum(x) - 1) > tolerance) {
    return("Must add up to 1")
  }
  return(TRUE)
}

#' @rdname check_probability_vector
#' @inheritParams checkmate::assert_atomic_vector
#' @inheritParams checkmate::assert_numeric
#' @export

assert_probability_vector <- checkmate::makeAssertionFunction(
  check_probability_vector
)

#' @rdname check_probability_vector
#' @inheritParams checkmate::assert_atomic_vector
#' @inheritParams checkmate::assert_numeric
#' @export

test_probability_vector <- checkmate::makeTestFunction(
  check_probability_vector
)
