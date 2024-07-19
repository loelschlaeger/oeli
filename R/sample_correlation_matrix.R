#' Sample correlation matrix
#'
#' @description
#' This function samples a correlation matrix by sampling a covariance matrix
#' from an inverse Wishart distribution and transforming it to a correlation
#' matrix.
#'
#' @param dim \[`integer(1)`\]\cr
#' The dimension.
#'
#' @param df \[`integer(1)`\]\cr
#' The degrees of freedom of the inverse Wishart distribution greater or equal
#' \code{dim}.
#'
#' @param scale \[`matrix()`\]\cr
#' The scale covariance matrix of the inverse Wishart distribution of dimension
#' \code{dim}.
#'
#' @return
#' A correlation \code{matrix}.
#'
#' @keywords simulation
#' @family matrix helpers
#' @export
#'
#' @examples
#' sample_correlation_matrix(dim = 3)

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
