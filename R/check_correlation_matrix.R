#' Check correlation matrix
#'
#' @description
#' These functions check whether the input fulfills the properties of a
#' correlation matrix.
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
#' M <- matrix(c(1,  0.9,  0.9, 0.9,  1,  -0.9, 0.9,  -0.9,  1), nrow = 3)
#' check_correlation_matrix(M)
#' test_correlation_matrix(M)
#' \dontrun{
#' assert_correlation_matrix(M)
#' }

check_correlation_matrix <- function(
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
  if (any(abs(x - t(x)) > tolerance)) {
    return("Must be symmetric")
  }
  if (any(abs(diag(x) - 1) > tolerance)) {
    return("Must have ones on the diagonal")
  }
  if (any(x < -1 | x > 1)) {
    return("Must have values between -1 and 1")
  }
  if (any(eigen(x)$value < -tolerance)) {
    return("Must have positive eigenvalues only")
  }
  if (!is.null(dim)) {
    input_check_response(
      check = checkmate::check_count(dim, positive = TRUE),
      var_name = "dim"
    )
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
