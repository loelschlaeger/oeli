#' Get Cholesky root elements and build covariance matrix
#'
#' @description
#' These functions compute the Cholesky root elements of a covariance matrix
#' and, conversely, build a covariance matrix from its Cholesky root elements.
#'
#' @param cov
#' A covariance \code{matrix} of dimension \code{dim}.
#' @param chol
#' A \code{numeric} \code{vector} of Cholesky root elements.
#' @param unique
#' Set to \code{TRUE} to ensure that the Cholesky decomposition is unique
#' by restricting the diagonal elements to be positive.
#'
#' @return
#' For \code{\link{cov_2_chol}} a \code{numeric} \code{vector} of Cholesky root
#' elements.
#' For \code{\link{chol_2_cov}} a covariance \code{matrix}.
#'
#' @examples
#' cov <- sample_covariance_matrix(4)
#' chol <- cov_2_chol(cov)
#' all.equal(cov, chol_2_cov(chol))
#'
#' @export

cov_2_chol <- function(cov, unique = TRUE) {
  assert_covariance_matrix(cov)
  checkmate::assert_flag(unique)
  cov_chol <- t(chol(cov))
  diag(cov_chol) <- abs(diag(cov_chol))
  chol <- cov_chol[lower.tri(cov_chol, diag = TRUE)]
  if (unique) {
    return(unique_chol(chol))
  } else {
    return(chol)
  }
}

#' @rdname cov_2_chol
#' @export

chol_2_cov <- function(chol) {
  checkmate::assert_vector(chol, any.missing = FALSE)
  checkmate::assert_numeric(chol, finite = TRUE)
  dim <- -0.5 + sqrt(0.25 + 2 * length(chol))
  checkmate::assert_count(dim, positive = TRUE)
  cov <- matrix(0, dim, dim)
  cov[lower.tri(cov, diag = TRUE)] <- chol
  cov %*% t(cov)
}

#' @rdname cov_2_chol
#' @export

unique_chol <- function(chol) {
  checkmate::assert_vector(chol, any.missing = FALSE)
  checkmate::assert_numeric(chol, finite = TRUE)
  if (length(chol) == 0) {
    return(numeric(0))
  }
  dim <- -0.5 + sqrt(0.25 + 2 * length(chol))
  checkmate::assert_count(dim, positive = TRUE)
  root <- matrix(0, dim, dim)
  root[lower.tri(root, diag = TRUE)] <- chol
  diagonal <- diag(root)
  for (i in seq_along(diagonal)) {
    if (diagonal[i] < 0) root[, i] <- -root[, i]
  }
  root[lower.tri(root, diag = TRUE)]
}
