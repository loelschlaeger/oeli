#' Check covariance matrix
#'
#' @description
#' These functions check whether the input fulfills the properties of a
#' covariance matrix.
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
#' M <- matrix(c(1, 2, 3, 2, 1, 2, 3, 2, 1), nrow = 3)
#' check_covariance_matrix(M)
#' test_covariance_matrix(M)
#' \dontrun{
#' assert_covariance_matrix(M)
#' }

check_covariance_matrix <- function(
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
