#' Cholesky root of covariance matrix
#'
#' @description
#' These functions compute the Cholesky root elements of a covariance matrix
#' and, conversely, build a covariance matrix from its Cholesky root elements.
#'
#' @param cov \[`matrix()`\]\cr
#' A covariance matrix.
#'
#' It can also be the zero matrix, in which case the Cholesky root is defined as
#' the zero matrix.
#'
#' @param chol \[`numeric()`\]\cr
#' Cholesky root elements.
#'
#' @param unique \[`logical(1)`\]\cr
#' Ensure that the Cholesky decomposition is unique by restricting the diagonal
#' elements to be positive?
#'
#' @return
#' For \code{\link{cov_to_chol}} a \code{numeric} \code{vector} of Cholesky root
#' elements.
#'
#' For \code{\link{chol_to_cov}} a covariance \code{matrix}.
#'
#' @keywords transformation
#' @family matrix helpers
#' @export
#'
#' @examples
#' cov <- sample_covariance_matrix(4)
#' chol <- cov_to_chol(cov)
#' all.equal(cov, chol_to_cov(chol))

cov_to_chol <- function(cov, unique = TRUE) {
  input_check_response(
    check_covariance_matrix(cov),
    "cov"
  )
  input_check_response(
    checkmate::check_flag(unique),
    "unique"
  )
  if (all(as.vector(cov) == 0)) {
    cov_chol <- cov
  } else {
    cov_chol <- t(chol(cov))
  }
  chol <- cov_chol[lower.tri(cov_chol, diag = TRUE)]
  if (unique) {
    return(unique_chol(chol))
  } else {
    return(chol)
  }
}

#' @rdname cov_to_chol
#' @export

chol_to_cov <- function(chol) {
  input_check_response(
    check_numeric_vector(chol, any.missing = FALSE, finite = TRUE),
    "chol"
  )
  dim <- -0.5 + sqrt(0.25 + 2 * length(chol))
  input_check_response(
    checkmate::check_count(dim, positive = TRUE),
    prefix = "Length of {.var chol} is bad:"
  )
  cov <- matrix(0, dim, dim)
  cov[lower.tri(cov, diag = TRUE)] <- chol
  cov %*% t(cov)
}

#' @rdname cov_to_chol
#' @export

unique_chol <- function(chol) {
  input_check_response(
    check_numeric_vector(chol, any.missing = FALSE, finite = TRUE),
    "chol"
  )
  if (length(chol) == 0) {
    return(numeric(0))
  }
  dim <- -0.5 + sqrt(0.25 + 2 * length(chol))
  input_check_response(
    checkmate::check_count(dim, positive = TRUE),
    prefix = "Length of {.var chol} is bad:"
  )
  root <- matrix(0, dim, dim)
  root[lower.tri(root, diag = TRUE)] <- chol
  diagonal <- diag(root)
  for (i in seq_along(diagonal)) {
    if (diagonal[i] < 0) root[, i] <- -root[, i]
  }
  root[lower.tri(root, diag = TRUE)]
}
