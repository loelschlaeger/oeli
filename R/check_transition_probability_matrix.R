#' Check transition probability matrix
#'
#' @description
#' These functions check whether the input is a transition probability matrix.
#'
#' @inheritParams check_covariance_matrix
#'
#' @return
#' Same as documented in \code{\link[checkmate]{check_matrix}}.
#'
#' @keywords validation
#' @family matrix helpers
#' @export
#'
#' @examples
#' T <- matrix(c(0.8,  0.2,  0.1, 0.1,  0.7,  0.4, 0.1,  0.1,  0.6), nrow = 3)
#' check_transition_probability_matrix(T)
#' test_transition_probability_matrix(T)
#' \dontrun{
#' assert_transition_probability_matrix(T)
#' }

check_transition_probability_matrix <- function(
    x, dim = NULL, tolerance = sqrt(.Machine$double.eps)
  ) {
  input_check_response(
    check = checkmate::check_number(tolerance, lower = 0),
    var_name = "tolerance"
  )
  res <- checkmate::check_matrix(x, mode = "numeric")
  if (!isTRUE(res)) {
    return(res)
  }
  if (nrow(x) != ncol(x)) {
    return("Must be square")
  }
  if (any(is.na(x))) {
    return("Must not have NA values")
  }
  if (any(!is.finite(x))) {
    return("Must not have infinite values")
  }
  if (any(x < 0 | x > 1)) {
    return("Must have values between 0 and 1")
  }
  if (any(abs(rowSums(x) - 1) > tolerance)) {
    return("Must have row sums equal to 1")
  }
  if (!is.null(dim)) {
    checkmate::assert_count(dim, positive = TRUE)
    if (nrow(x) != dim) {
      return(paste("Must be of dimension", dim))
    }
  }
  return(TRUE)
}

#' @rdname check_transition_probability_matrix
#' @inheritParams checkmate::assert_matrix
#' @export

assert_transition_probability_matrix <- checkmate::makeAssertionFunction(
  check_transition_probability_matrix
)

#' @rdname check_transition_probability_matrix
#' @inheritParams checkmate::test_matrix
#' @export

test_transition_probability_matrix <- checkmate::makeTestFunction(
  check_transition_probability_matrix
)
