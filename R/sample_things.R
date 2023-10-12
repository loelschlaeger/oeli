# These functions draw from distributions. They base on C++ implementation
# but provide additional input checks.

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
  if (diag) cov[row(cov) != col(cov)] <- 0
  assert_covariance_matrix(cov)
  return(cov)
}

#' @inherit rdirichlet_cpp title description
#' @inheritParams rdirichlet_cpp
#'
#' @param n
#' An \code{integer}, the number of samples.
#'
#' @return
#' If \code{n = 1} a \code{vector} of length \code{p}, else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
#'
#' @examples
#' rdirichlet(concentration = 1:3)
#' rdirichlet(n = 4, concentration = 1:2)
#'
#' @export

rdirichlet <- function(n = 1, concentration) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_vector(concentration, strict = TRUE)
  checkmate::assert_numeric(concentration, any.missing = FALSE, lower = 0)
  out <- replicate(n = n, rdirichlet_cpp(concentration), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else {
    t(out)
  }
}

#' @inherit rmvnorm_cpp title description
#' @inheritParams rmvnorm_cpp
#'
#' @param n
#' An \code{integer}, the number of samples.
#'
#' @return
#' If \code{n = 1} a \code{vector} of length \code{p}, else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
#'
#' @examples
#' mean <- c(0, 0)
#' Sigma <- diag(2)
#' rmvnorm(n = 3, mean = mean, Sigma = Sigma)
#' rmvnorm(mean = mean, Sigma = Sigma, log = TRUE)
#'
#' @export

rmvnorm <- function(n = 1, mean, Sigma, log = FALSE) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_vector(mean, strict = TRUE)
  checkmate::assert_numeric(mean, finite = TRUE, any.missing = FALSE)
  assert_covariance_matrix(Sigma, dim = length(mean))
  checkmate::assert_flag(log)
  out <- replicate(n = n, rmvnorm_cpp(mean, Sigma, log), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else {
    t(out)
  }
}

#' @inherit rwishart_cpp title description return
#' @inheritParams rwishart_cpp
#'
#' @examples
#' df <- 4
#' scale <- diag(2)
#' rwishart(df = df, scale = scale)
#' rwishart(df = df, scale = scale, inv = TRUE)
#'
#' @export

rwishart <- function(df, scale, inv = FALSE) {
  assert_covariance_matrix(scale)
  checkmate::assert_int(df, lower = nrow(scale))
  checkmate::assert_flag(inv)
  rwishart_cpp(df, scale, inv)
}

