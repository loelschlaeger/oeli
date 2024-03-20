#' Check if an argument is a numeric vector
#'
#' @description
#' This function checks whether the input is a numeric vector.
#'
#' @param x
#' Object to check.
#'
#' @inheritParams checkmate::check_numeric
#' @inheritParams checkmate::check_atomic_vector
#'
#' @return
#' Compare to \code{\link[checkmate]{check_numeric}}.
#'
#' @export

check_numeric_vector <- function(
    x, lower = -Inf, upper = Inf, finite = FALSE, any.missing = TRUE,
    all.missing = TRUE, len = NULL, min.len = NULL, max.len = NULL,
    unique = FALSE, sorted = FALSE, names = NULL, typed.missing = FALSE,
    null.ok = FALSE
) {
  if (isTRUE(null.ok) && is.null(x)) {
    return(TRUE)
  }
  res1 <- checkmate::check_atomic_vector(
    x, any.missing = any.missing, all.missing = all.missing, len = len,
    min.len = min.len, max.len = max.len, unique = unique, names = names
  )
  if (!isTRUE(res1)) {
    return(res1)
  }
  res2 <- checkmate::check_numeric(
    x, lower = lower, upper = upper, finite = finite, any.missing = any.missing,
    all.missing = all.missing, len = len, min.len = min.len, max.len = max.len,
    unique = unique, sorted = sorted, names = names,
    typed.missing = typed.missing, null.ok = null.ok
  )
  if (!isTRUE(res2)) {
    return(res2)
  }
  return(TRUE)
}

#' @rdname check_numeric_vector
#' @inheritParams checkmate::assert_numeric
#' @inheritParams checkmate::assert_atomic_vector
#' @export

assert_numeric_vector <- checkmate::makeAssertionFunction(
  check_numeric_vector
)

#' @rdname check_numeric_vector
#' @inheritParams checkmate::assert_numeric
#' @inheritParams checkmate::assert_atomic_vector
#' @export

test_numeric_vector <- checkmate::makeTestFunction(
  check_numeric_vector
)
