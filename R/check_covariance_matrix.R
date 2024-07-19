#' Check if an argument is a covariance matrix
#'
#' @description
#' This function checks whether the input is a symmetric, real matrix that
#' fulfills the covariance matrix properties.
#'
#' @inheritParams checkmate::check_matrix
#'
#' @param dim \[`integer(1)`\]\cr
#' The matrix dimension.
#'
#' @param tolerance \[`numeric(1)`\]\cr
#' A non-negative tolerance value.
#'
#' @return
#' Same as documented in \code{\link[checkmate]{check_matrix}}.
#'
#' @keywords validation
#' @family matrix helpers
#' @export
#'
#' @examples
#' # TODO

check_covariance_matrix <- function(
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
  if (any(abs(x - t(x)) > tolerance)) {
    return("Must be symmetric")
  }
  if (any(eigen(x)$value < -tolerance)) {
    return("Must have positive eigenvalues only")
  }
  if (!is.null(dim)) {
    checkmate::assert_count(dim, positive = TRUE)
    if (nrow(x) != dim) {
      return(paste("Must be of dimension", dim))
    }
  }
  return(TRUE)
}

#' @rdname check_covariance_matrix
#' @inheritParams checkmate::assert_matrix
#' @export

assert_covariance_matrix <- checkmate::makeAssertionFunction(
  check_covariance_matrix
)

#' @rdname check_covariance_matrix
#' @inheritParams checkmate::test_matrix
#' @export

test_covariance_matrix <- checkmate::makeTestFunction(
  check_covariance_matrix
)
