#' Sample covariance matrix
#'
#' @description
#' This function samples a covariance matrix from an inverse Wishart
#' distribution.
#'
#' @param dim
#' An \code{integer}, the dimension.
#' @param df
#' An \code{integer} greater or equal \code{dim}, the degrees of freedom of
#' the inverse Wishart distribution.
#' @param scale
#' A covariance \code{matrix} of dimension \code{dim}, the scale matrix of the
#' inverse Wishart distribution.
#' @param diag
#' Set to \code{TRUE} for a diagonal matrix.
#'
#' @return
#' A covariance \code{matrix}.
#'
#' @examples
#' sample_covariance_matrix(dim = 3)
#'
#' @export

sample_covariance_matrix <- function(
    dim, df = dim, scale = diag(dim), diag = FALSE
  ) {
  checkmate::assert_count(dim, positive = TRUE)
  checkmate::assert_int(df, lower = dim)
  assert_covariance_matrix(scale, dim = dim)
  checkmate::assert_flag(diag)
  cov <- solve(stats::rWishart(1, df = df, Sigma = scale)[,,1])
  if(diag) cov[row(cov) != col(cov)] <- 0
  assert_covariance_matrix(cov)
  cov
}
