#' Get indices of matrix diagonal
#'
#' @description
#' This function returns the indices of the diagonal elements of a quadratic
#' matrix.
#'
#' @param n \[`integer(1)`\]\cr
#' The matrix dimension.
#'
#' @param triangular \[`NULL` or `character(1)`\]\cr
#' If \code{NULL} (default), all elements of the matrix are considered. If
#' \code{"lower"} (\code{"upper"}), only the lower- (upper-) triangular matrix
#' is considered.
#'
#' @return
#' An \code{integer} \code{vector}.
#'
#' @export
#' @keywords indexing
#' @family matrix helpers
#'
#' @examples
#' # indices of diagonal elements
#' n <- 3
#' matrix(1:n^2, n, n)
#' matrix_diagonal_indices(n)
#'
#' # indices of diagonal elements of lower-triangular matrix
#' L <- matrix(0, n, n)
#' L[lower.tri(L, diag=TRUE)] <- 1:((n * (n + 1)) / 2)
#' L
#' matrix_diagonal_indices(n, triangular = "lower")
#'
#' # indices of diagonal elements of upper-triangular matrix
#' U <- matrix(0, n, n)
#' U[upper.tri(U, diag=TRUE)] <- 1:((n * (n + 1)) / 2)
#' U
#' matrix_diagonal_indices(n, triangular = "upper")

matrix_diagonal_indices <- function(n, triangular = NULL) {
  checkmate::assert_int(n, lower = 1)
  if (is.null(triangular)) {
    M <- matrix(1:n^2, n, n)
  } else {
    M <- matrix(0, n, n)
    triangular <- match_arg(triangular, c("lower", "upper"))
    if (triangular == "lower") {
      ind <- lower.tri(M, diag = TRUE)
    } else {
      ind <- upper.tri(M, diag = TRUE)
    }
    M[ind] <- 1:((n * (n + 1)) / 2)
  }
  as.integer(diag(M))
}
