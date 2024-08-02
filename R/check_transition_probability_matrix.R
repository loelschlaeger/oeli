#' Check if an argument is a transition probability matrix
#'
#' @description
#' This function checks whether the input is a quadratic, real matrix with
#' elements between 0 and 1 and row sums equal to 1.
#'
#' @inheritParams check_covariance_matrix
#'
#' @return
#' Same as documented in \code{\link[checkmate]{check_matrix}}.
#'
#' @keywords validation
#' @family matrix helpers
#' @export

check_transition_probability_matrix <- function(
    x, dim = NULL, tolerance = sqrt(.Machine$double.eps)
  ) {
  checkmate::assert_number(tolerance, lower = 0)
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