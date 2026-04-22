#' Check one-hot matrix
#'
#' @description
#' These functions check whether the input is a one-hot matrix, i.e., a
#' `numeric` matrix where each row contains exactly one entry equal to `1` and
#' all other entries equal to `0`.
#'
#' @inheritParams check_covariance_matrix
#' @inheritParams checkmate::check_matrix
#'
#' @return
#' Same as documented in \code{\link[checkmate]{check_matrix}}.
#'
#' @keywords validation
#' @family matrix helpers
#' @export
#'
#' @examples
#' x <- matrix(c(
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 0, 0, 0
#' ), nrow = 3, byrow = TRUE)
#'
#' check_one_hot_matrix(x)
#' test_one_hot_matrix(x)
#' \dontrun{
#' assert_one_hot_matrix(x)
#' }

check_one_hot_matrix <- function(
    x, nrows = NULL, ncols = NULL, tolerance = sqrt(.Machine$double.eps)
) {
  input_check_response(
    check = checkmate::check_number(tolerance, lower = 0),
    var_name = "tolerance"
  )
  res <- checkmate::check_matrix(
    x, mode = "numeric", nrows = nrows, ncols = ncols
  )
  if (!isTRUE(res)) {
    return(res)
  }
  if (anyNA(x)) {
    return("Must not contain missing values")
  }
  if (any(!is.finite(x))) {
    return("Must not contain infinite values")
  }
  is_zero <- abs(x) <= tolerance
  is_one <- abs(x - 1) <= tolerance
  if (any(!(is_zero | is_one))) {
    return("All elements must be 0 or 1")
  }
  if (any(rowSums(is_one) != 1L)) {
    return("Each row must contain exactly one 1")
  }
  return(TRUE)
}

#' @rdname check_one_hot_matrix
#' @inheritParams checkmate::assert_matrix
#' @export

assert_one_hot_matrix <- checkmate::makeAssertionFunction(
  check_one_hot_matrix
)

#' @rdname check_one_hot_matrix
#' @inheritParams checkmate::test_matrix
#' @export

test_one_hot_matrix <- checkmate::makeTestFunction(
  check_one_hot_matrix
)
