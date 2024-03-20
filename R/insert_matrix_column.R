#' Insert column in matrix
#'
#' @description
#' This function inserts a column into a matrix.
#'
#' @param A
#' A \code{matrix}.
#'
#' @param x
#' A \code{vector} of length \code{nrow(A)}, the column to be added.
#'
#' Can also be a single value.
#'
#' @param p
#' An \code{integer}, the position where to add the column:
#' - \code{p = 0} appends the column left
#' - \code{p = ncol(A)} appends the column right
#' - \code{p = n} inserts the column between the \code{n}-th and
#'   \code{(n + 1)}-th column of \code{A}.
#'
#' Can also be a \code{vector} of multiple positions.
#'
#' @return
#' A \code{matrix}.
#'
#' @export
#'
#' @examples
#' A <- diag(3)
#' x <- 1:3
#' insert_matrix_column(A, x, 0)
#' insert_matrix_column(A, x, 1)
#' insert_matrix_column(A, x, 2)
#' insert_matrix_column(A, x, 3)
#'
#' ### also single value
#' x <- 2
#' insert_matrix_column(A, x, 0)
#'
#' ### also multiple positions
#' insert_matrix_column(A, x, 0:3)
#'
#' ### also trivial case
#' insert_matrix_column(matrix(nrow = 0, ncol = 0), integer(), integer())

insert_matrix_column <- function(A, x, p) {
  checkmate::assert_matrix(A)
  n <- ncol(A)
  checkmate::assert_atomic_vector(x)
  stopifnot(
    "'x' must be of length 1 or nrow(A)" = length(x) == 1 || length(x) == nrow(A)
  )
  checkmate::assert_integerish(
    p, lower = 0, upper = n, unique = TRUE, max.len = n + 1
  )
  if (length(p) == 0) {
    return(A)
  }
  p <- sort(p)
  for (i in seq_along(p)) {
    if (p[i] == 0) {
      A <- cbind(x, A, deparse.level = 0)
    } else if (p[i] == n) {
      A <- cbind(A, x, deparse.level = 0)
    } else {
      A <- cbind(
        A[, seq_len(p[i]), drop = FALSE], x, A[, (p[i] + 1):n, drop = FALSE],
        deparse.level = 0
      )
    }
    p <- p + 1
    n <- ncol(A)
  }
  return(A)
}
