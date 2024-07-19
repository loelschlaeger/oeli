#' Sample covariance matrix
#'
#' @description
#' This function samples a covariance matrix from an inverse Wishart
#' distribution.
#'
#' @inheritParams sample_correlation_matrix
#'
#' @param diag \[`logical(1)`\]\cr
#' Diagonal matrix?
#'
#' @return
#' A covariance \code{matrix}.
#'
#' @keywords simulation
#' @family matrix helpers
#' @export
#'
#' @examples
#' sample_covariance_matrix(dim = 3)

sample_covariance_matrix <- function(
    dim, df = dim, scale = diag(dim), diag = FALSE) {
  checkmate::assert_count(dim, positive = TRUE)
  checkmate::assert_int(df, lower = dim)
  assert_covariance_matrix(scale, dim = dim)
  checkmate::assert_flag(diag)
  cov <- rwishart(df = df, scale = scale, inv = TRUE)
  if (diag) cov[row(cov) != col(cov)] <- 0
  assert_covariance_matrix(cov)
  return(cov)
}
