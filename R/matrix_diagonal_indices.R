#' Get indices of matrix diagonal
#'
#' @description
#' This function returns the indices of the diagonal elements of a quadratic
#' matrix.
#'
#' @param n
#' An \code{integer}, the matrix dimension.
#'
#' @return
#' An \code{integer} \code{vector}.
#'
#' @examples
#' n <- 4
#' matrix(1:n^2, n, n)
#' matrix_diagonal_indices(n)
#'
#' @export

matrix_diagonal_indices <- function(n) {
  which(diag(n) == 1)
}
