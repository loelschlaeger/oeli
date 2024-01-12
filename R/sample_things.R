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
  dim <- length(concentration)
  out <- replicate(n = n, rdirichlet_cpp(concentration), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else if (dim == 1) {
    as.matrix(out)
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
  dim <- length(mean)
  if (dim == 1 && checkmate::test_atomic_vector(Sigma, len = 1)) {
    Sigma <- as.matrix(Sigma)
  }
  assert_covariance_matrix(Sigma, dim = dim)
  checkmate::assert_flag(log)
  out <- replicate(n = n, rmvnorm_cpp(mean, Sigma, log), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else if (dim == 1) {
    as.matrix(out)
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

#' Sample transition probability matrices
#'
#' @description
#' This function returns a random, squared matrix of dimension \code{dim}
#' that fulfills the properties of a transition probability matrix.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#' @param state_persistent
#' Set to \code{TRUE} (default) to put more probability on the diagonal.
#'
#' @return
#' A transition probability \code{matrix}.
#'
#' @importFrom stats runif
#'
#' @examples
#' sample_transition_probability_matrix(dim = 3)
#'
#' @export

sample_transition_probability_matrix <- function(dim, state_persistent = TRUE) {
  checkmate::assert_int(dim, lower = 1)
  checkmate::assert_flag(state_persistent)
  Gamma <- matrix(stats::runif(dim^2), dim, dim)
  if (state_persistent) Gamma <- Gamma + diag(dim)
  Gamma / rowSums(Gamma)
}

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
    dim, df = dim, scale = diag(dim), diag = FALSE) {
  checkmate::assert_count(dim, positive = TRUE)
  checkmate::assert_int(df, lower = dim)
  assert_covariance_matrix(scale, dim = dim)
  checkmate::assert_flag(diag)
  cov <- solve(stats::rWishart(1, df = df, Sigma = scale)[, , 1])
  if (diag) cov[row(cov) != col(cov)] <- 0
  assert_covariance_matrix(cov)
  return(cov)
}
