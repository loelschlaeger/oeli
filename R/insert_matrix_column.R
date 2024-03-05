#' Add a column to a matrix
#'
#' @description
#' This function adds a column to a matrix.
#'
#' @param A
#' A \code{matrix}.
#'
#' @param x
#' A \code{numeric} \code{vector} of length \code{nrow(A)}, the column to be
#' added.
#'
#' @param p
#' An \code{integer}, the position where to add the column:
#' - \code{p = 0} appends the column left
#' - \code{p = ncol(A)} appends the column right
#' - \code{p = n} inserts the column between the \code{n}-th and
#'   \code{(n + 1)}-th column of \code{A}.
#'
#' @return
#' A \code{matrix}.
#'
#' @export
#'
#' @examples
#' A <- diag(3)
#' x <- numeric(3)
#' insert_matrix_column(A, x, 0)
#' insert_matrix_column(A, x, 1)
#' insert_matrix_column(A, x, 2)
#' insert_matrix_column(A, x, 3)

insert_matrix_column <- function(A, x, p) {
  checkmate::assert_matrix(A, mode = "numeric", min.rows = 1, min.cols = 1)
  n <- ncol(A)
  m <- nrow(A)
  assert_numeric_vector(x, len = m)
  checkmate::assert_int(p, lower = 0, upper = n)
  if (p == 0) {
    cbind(x, A, deparse.level = 0)
  } else if (p == n) {
    cbind(A, x, deparse.level = 0)
  } else {
    cbind(A[, seq_len(p)], x, A[, (p + 1):n], deparse.level = 0)
  }
}
