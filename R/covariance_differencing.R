#' Difference and un-difference covariance matrix
#'
#' @description
#' These functions difference and un-difference a covariance matrix with respect
#' to row \code{ref}.
#'
#' @param cov,cov_diff
#' A (differenced) covariance \code{matrix} of dimension \code{dim}
#' (or \code{dim} - 1, respectively).
#' @param ref
#' An \code{integer} between \code{1} and \code{dim}, the reference row for
#' differencing that maps \code{cov} to \code{cov_diff}, see details.
#' By default, \code{ref = 1}.
#' @param dim
#' An \code{integer}, the dimension.
#'
#' @return
#' A (differenced or un-differenced) covariance \code{matrix}.
#'
#' @details
#' For differencing: Let \eqn{\Sigma} be a covariance matrix of dimension
#' \eqn{n}. Then \deqn{\tilde{\Sigma} = \Delta_k \Sigma \Delta_k'}
#' is the differenced covariance matrix with respect to row \eqn{k = 1,\dots,n},
#' where \eqn{\Delta_k} is a difference operator that depends on the reference
#' row \eqn{k}. More precise, \eqn{\Delta_k} the identity matrix of dimension
#' \eqn{n} without row \eqn{k} and with \eqn{-1}s in column \eqn{k}.
#' It can be computed with \code{delta(ref = k, dim = n)}.
#'
#' For un-differencing: The "un-differenced" covariance matrix \eqn{\Sigma}
#' cannot be uniquely computed from \eqn{\tilde{\Sigma}}.
#' For a non-unique solution, we add a column and a row of zeros
#' at column and row number \eqn{k} to \eqn{\tilde{\Sigma}}, respectively, and
#' add \eqn{1} to each matrix entry to make the result a proper covariance
#' matrix.
#'
#' @examples
#' n <- 3
#' Sigma <- sample_covariance_matrix(dim = n)
#' k <- 2
#'
#' # build difference operator
#' delta(ref = k, dim = n)
#'
#' # difference Sigma
#' (Sigma_diff <- diff_cov(Sigma, ref = k))
#'
#' # un-difference Sigma
#' undiff_cov(Sigma_diff, ref = k)
#'
#' @export

diff_cov <- function(cov, ref = 1) {
  assert_covariance_matrix(cov)
  dim <- nrow(cov)
  checkmate::assert_int(ref, lower = 1, upper = dim)
  D <- delta(ref = ref, dim = dim)
  D %*% cov %*% t(D)
}

#' @rdname diff_cov
#' @export

undiff_cov <- function(cov_diff, ref = 1) {
  assert_covariance_matrix(cov_diff)
  dim <- nrow(cov_diff) + 1
  checkmate::assert_int(ref, lower = 1, upper = dim)
  cov <- matrix(0, dim, dim)
  cov[row(cov) != ref & col(cov) != ref] <- cov_diff
  cov <- cov + 1
  assert_covariance_matrix(cov)
  return(cov)
}

#' @rdname diff_cov
#' @export

delta <- function(ref = 1, dim) {
  checkmate::assert_int(dim, lower = 2)
  checkmate::assert_int(ref, lower = 1, upper = dim)
  D <- diag(dim)
  D[, ref] <- -1
  D[-ref, , drop = FALSE]
}
