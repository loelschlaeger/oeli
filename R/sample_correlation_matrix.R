#' Sample correlation matrix
#'
#' @description
#' This function samples a correlation matrix by sampling a covariance matrix
#' from an inverse Wishart distribution and transforming it to a correlation
#' matrix.
#'
#' @param dim
#' An \code{integer}, the dimension.
#' @param df
#' An \code{integer} greater or equal \code{dim}, the degrees of freedom of
#' the inverse Wishart distribution.
#' @param scale
#' A covariance \code{matrix} of dimension \code{dim}, the scale matrix of the
#' inverse Wishart distribution.
#'
#' @return
#' A correlation \code{matrix}.
#'
#' @examples
#' sample_correlation_matrix(dim = 3)
#'
#' @export

sample_correlation_matrix <- function(
    dim, df = dim, scale = diag(dim)
  ) {
  cov <- sample_covariance_matrix(
    dim = dim, df = df, scale = scale, diag = FALSE
  )
  D <- diag(1 / sqrt(diag(cov)))
  cor <- D %*% cov %*% D
  diag(cor) <- 1
  cor[cor > 1] <- 1
  cor[cor < -1] <- -1
  assert_correlation_matrix(cor)
  return(cor)
}
