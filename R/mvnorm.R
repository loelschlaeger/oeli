#' @inherit dmvnorm_cpp title description return
#' @inheritParams dmvnorm_cpp
#'
#' @examples
#' x <- c(0, 0)
#' mean <- c(0, 0)
#' Sigma <- diag(2)
#' dmvnorm(x = x, mean = mean, Sigma = Sigma)
#' dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
#'
#' @export

dmvnorm <- function(x, mean, Sigma, log = FALSE) {
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(mean, any.missing = FALSE, len = length(x))
  assert_covariance_matrix(Sigma, dim = length(x))
  checkmate::assert_flag(log)
  dmvnorm_cpp(x, mean, Sigma, log)
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
