#' Check probability vector
#'
#' @description
#' These functions check whether the input fulfills the properties of a
#' probability matrix.
#'
#' @param tolerance \[`numeric(1)`\]\cr
#' A non-negative tolerance value.
#'
#' @inheritParams checkmate::check_numeric
#'
#' @return
#' Same as documented in \code{\link[checkmate]{check_numeric}}.
#'
#' @keywords validation
#' @family vector helpers
#' @export
#'
#' @examples
#' p <- c(0.2, 0.3, 0.6)
#' check_probability_vector(p)
#' test_probability_vector(p)
#' \dontrun{
#' assert_probability_vector(p)
#' }

check_probability_vector <- function(
    x, len = NULL, tolerance = sqrt(.Machine$double.eps)
) {
  input_check_response(
    check = checkmate::check_number(tolerance, lower = 0),
    var_name = "tolerance"
  )
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
