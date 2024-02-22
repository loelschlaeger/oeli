#' Check if an argument is a correlation matrix
#'
#' @description
#' This function checks whether the input is a symmetric, real matrix that
#' fulfills the correlation matrix properties.
#'
#' @param x
#' Object to check.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#'
#' @param tolerance
#' A non-negative \code{numeric} tolerance value.
#'
#' @return
#' Compare to \code{\link[checkmate]{check_matrix}}.
#'
#' @export

check_correlation_matrix <- function(
    x, dim = NULL, tolerance = sqrt(.Machine$double.eps)) {
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
  if (any(abs(diag(x) - 1) > tolerance)) {
    return("Must have ones on the diagonal")
  }
  if (any(x < -1 | x > 1)) {
    return("Must have values between -1 and 1")
  }
  if (!is.null(dim)) {
    checkmate::assert_count(dim, positive = TRUE)
    if (nrow(x) != dim) {
      return(paste("Must be of dimension", dim))
    }
  }
  return(TRUE)
}

#' @rdname check_correlation_matrix
#' @inheritParams checkmate::assert_matrix
#' @export

assert_correlation_matrix <- checkmate::makeAssertionFunction(
  check_correlation_matrix
)

#' @rdname check_correlation_matrix
#' @inheritParams checkmate::test_matrix
#' @export
test_correlation_matrix <- checkmate::makeTestFunction(
  check_correlation_matrix
)
