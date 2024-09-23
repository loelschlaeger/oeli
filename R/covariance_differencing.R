#' Difference and un-difference covariance matrix
#'
#' @description
#' These functions difference and un-difference random vectors and covariance
#' matrices.
#'
#' @param cov,cov_diff \[`matrix()`\]\cr
#' A (differenced) covariance matrix of dimension \code{dim}
#' (or \code{dim} - 1, respectively).
#'
#' @param ref \[`integer(1)`\]\cr
#' The reference row between \code{1} and \code{dim} for
#' differencing that maps \code{cov} to \code{cov_diff}, see details.
#'
#' @param dim \[`integer(1)`\]\cr
#' The matrix dimension.
#'
#' @return
#' A (differenced or un-differenced) covariance \code{matrix}.
#'
#' @details
#' Assume \eqn{x \sim N(0, \Sigma)} is a multivariate normally distributed
#' random vector of dimension \eqn{n}. We may want to consider the differenced
#' vector \deqn{\tilde x = (x_1 - x_k, x_2 - x_k, \dots, x_n - x_k)',} excluding
#' the \eqn{k}th element (hence, \eqn{\tilde x} is of dimension
#' \eqn{(n - 1) \times 1}). Formally, \eqn{\tilde x = \Delta_k x}, where
#' \eqn{\Delta_k} is a difference operator that depends on the reference
#' row \eqn{k}. More precise, \eqn{\Delta_k} is the identity matrix of dimension
#' \eqn{n} without row \eqn{k} and with \eqn{-1}s in column \eqn{k}.
#' The difference operator \eqn{\Delta_k} can be computed via
#' \code{delta(ref = k, dim = n)}.
#'
#' Then, \eqn{\tilde x \sim N(0, \tilde \Sigma)}, where
#' \deqn{\tilde{\Sigma} = \Delta_k \Sigma \Delta_k'}
#' is the differenced covariance matrix with respect to row \eqn{k = 1,\dots,n}.
#' The differenced covariance matrix \eqn{\tilde \Sigma} can be computed via
#' \code{diff_delta(Sigma, ref = k)}.
#'
#' Since \eqn{\Delta_k} is a non-bijective mapping, \eqn{\Sigma} cannot be
#' uniquely restored from \eqn{\tilde \Sigma}. However, it is possible to
#' compute a non-unique solution \eqn{\Sigma_0}, such that
#' \eqn{\Delta_k \Sigma_0 \Delta_k = \tilde \Sigma}. For such a non-unique
#' solution, we add a column and a row of zeros
#' at column and row number \eqn{k} to \eqn{\tilde{\Sigma}}, respectively.
#' An "un-differenced" covariance matrix \eqn{\Sigma_0} can be computed via
#' \code{undiff_delta(Sigma_diff, ref = k)}.
#'
#' @keywords transformation
#' @family matrix helpers
#' @export
#'
#' @examples
#' n <- 3
#' Sigma <- sample_covariance_matrix(dim = n)
#' k <- 2
#' x <- c(1, 3, 2)
#'
#' # build difference operator
#' delta_k <- delta(ref = k, dim = n)
#'
#' # difference vector
#' delta_k %*% x
#'
#' # difference Sigma
#' (Sigma_diff <- diff_cov(Sigma, ref = k))
#'
#' # un-difference Sigma
#' (Sigma_0 <- undiff_cov(Sigma_diff, ref = k))
#'
#' # difference again
#' Sigma_diff_2 <- diff_cov(Sigma_0, ref = k)
#' all.equal(Sigma_diff, Sigma_diff_2)

diff_cov <- function(cov, ref = 1) {
  input_check_response(
    check = check_covariance_matrix(cov),
    var_name = "cov"
  )
  dim <- nrow(cov)
  input_check_response(
    check = checkmate::check_int(ref, lower = 1, upper = dim),
    var_name = "ref"
  )
  D <- delta(ref = ref, dim = dim)
  D %*% cov %*% t(D)
}

#' @rdname diff_cov
#' @export

undiff_cov <- function(cov_diff, ref = 1) {
  input_check_response(
    check = check_covariance_matrix(cov_diff),
    var_name = "cov_diff"
  )
  dim <- nrow(cov_diff) + 1
  input_check_response(
    check = checkmate::check_int(ref, lower = 1, upper = dim),
    var_name = "ref"
  )
  cov <- matrix(0, dim, dim)
  cov[row(cov) != ref & col(cov) != ref] <- cov_diff
  cov
}

#' @rdname diff_cov
#' @export

delta <- function(ref = 1, dim) {
  input_check_response(
    check = checkmate::check_int(dim, lower = 2),
    var_name = "dim"
  )
  input_check_response(
    check = checkmate::check_int(ref, lower = 1, upper = dim),
    var_name = "ref"
  )
  D <- diag(dim)
  D[, ref] <- -1
  D[-ref, , drop = FALSE]
}
