#' Argument matching
#'
#' @description
#' This function matches function arguments and is a modified version of
#' \code{\link[base]{match.arg}}.
#'
#' @param arg
#' A \code{character} (vector), the function argument.
#' @param choices
#' A \code{character} (vector) of allowed values for \code{arg}.
#' @param several.ok
#' Either \code{TRUE} if \code{arg} is allowed to have more than one element,
#' or \code{FALSE} else.
#' @param none.ok
#' Either \code{TRUE} if \code{arg} is allowed to have zero elements,
#' or \code{FALSE} else.
#'
#' @return
#' The un-abbreviated version of the exact or unique partial match if there is
#' one. Otherwise, an error is signaled if \code{several.ok} is \code{FALSE}
#' or \code{none.ok} is \code{FALSE}.
#' When \code{several.ok} is \code{TRUE} and (at least) one element of
#' \code{arg} has a match, all un-abbreviated versions of matches are returned.
#' When \code{none.ok} is \code{TRUE} and \code{arg} has zero elements,
#' \code{character(0)} is returned.
#'
#' @export

match_arg <- function (arg, choices, several.ok = FALSE, none.ok = FALSE) {
  checkmate::assert_character(arg)
  checkmate::assert_character(choices)
  checkmate::assert_flag(several.ok)
  checkmate::assert_flag(none.ok)
  arg_name <- deparse(substitute(arg))
  if (!several.ok && length(arg) > 1L) {
    cli::cli_abort(
      "{.var {arg_name}} must be of length 1.",
      call = NULL
    )
  }
  if (length(arg) == 0L) {
    if (none.ok) {
      return(character(0))
    } else {
      cli::cli_abort(
        "{.var {arg_name}} must be of length greater or equal 1.",
        call = NULL
      )
    }
  }
  i <- pmatch(arg, choices, nomatch = 0, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    cli::cli_abort(
      "{.var {arg_name}} {ifelse(none.ok, 'can', 'must')} be one
      {ifelse(several.ok, 'or more', '')} of {.val {choices}}.",
      call = NULL
    )
  }
  i <- i[i > 0L]
  choices[i]
}

#' Check if an argument is a covariance matrix
#'
#' @description
#' This function checks whether the input is a symmetric, real matrix that
#' fulfills the covariance matrix properties.
#'
#' @param x
#' Object to check.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#'
#' @return
#' Compare to \code{\link[checkmate]{check_matrix}}.
#'
#' @export

check_covariance_matrix <- function(x, dim = NULL) {
  res <- checkmate::check_matrix(x, mode = "numeric")
  if (!isTRUE(res))
    return(res)
  if (nrow(x) != ncol(x))
    return("Must be square")
  if (any(abs(x - t(x)) > sqrt(.Machine$double.eps)))
    return("Must be symmetric")
  if (any(eigen(x)$value < -sqrt(.Machine$double.eps)))
    return("Must have positive eigenvalues only")
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
#' @return
#' Compare to \code{\link[checkmate]{check_matrix}}.
#'
#' @export

check_correlation_matrix <- function(x, dim = NULL) {
  res <- checkmate::check_matrix(x, mode = "numeric")
  if (!isTRUE(res))
    return(res)
  if (nrow(x) != ncol(x))
    return("Must be square")
  if (any(abs(x - t(x)) > sqrt(.Machine$double.eps)))
    return("Must be symmetric")
  if (any(diag(x) != 1))
    return("Must have ones on the diagonal")
  if (any(x < -1 | x > 1))
    return("Must have values between -1 and 1")
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

#' Check if an argument is a transition probability matrix
#'
#' @description
#' This function checks whether the input is a quadratic, real matrix with
#' elements between 0 and 1 and row sums equal to 1.
#'
#' @param x
#' Object to check.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#'
#' @return
#' Compare to \code{\link[checkmate]{check_matrix}}.
#'
#' @export

check_transition_probability_matrix <- function(x, dim = NULL) {
  res <- checkmate::check_matrix(x, mode = "numeric")
  if (!isTRUE(res))
    return(res)
  if (nrow(x) != ncol(x))
    return("Must be square")
  if (any(x < 0 | x > 1))
    return("Must have values between 0 and 1")
  if (any(rowSums(x) != 1))
    return("Must have row sums equal to 1")
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

#' Check if an argument is a probability vector
#'
#' @description
#' This function checks whether the input is a real vector with non-negative
#' entries that add up to one.
#'
#' @param x
#' Object to check.
#'
#' @inheritParams checkmate::check_numeric
#'
#' @return
#' Compare to \code{\link[checkmate]{check_numeric}}.
#'
#' @export

check_probability_vector <- function(x, len = NULL) {
  res1 <- checkmate::check_atomic_vector(x, any.missing = FALSE, len = len)
  if (!isTRUE(res1))
    return(res1)
  res2 <- checkmate::check_numeric(x, lower = 0, upper = 1)
  if (!isTRUE(res2))
    return(res2)
  if (sum(x) != 1)
    return("Must add up to 1")
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
