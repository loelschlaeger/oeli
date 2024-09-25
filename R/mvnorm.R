#' Multivariate normal distribution
#'
#' @description
#' The function `dmvnorm()` computes the density of a multivariate normal
#' distribution.
#'
#' The function `rmvnorm()` samples from a multivariate normal distribution.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' The univariate normal distribution is available as the special case `p = 1`.
#'
#' @param x \[`numeric()`\]\cr
#' A quantile vector of length `p`.
#'
#' @param mean \[`numeric()`\]\cr
#' The mean vector of length `p`.
#'
#' For `dmvnorm()` and `rmvnorm()`, it can also be of length `1` for
#' convenience, then `rep(mean, p)` is considered.
#'
#' @param Sigma \[`matrix()`\]\cr
#' The covariance matrix of dimension `p`.
#'
#' It can also be a zero matrix.
#'
#' For `dmvnorm()` and `rmvnorm()` and if `p = 1`, it can also be a single
#' `numeric` for convenience. Note that `Sigma` is this case is a variance,
#' which is a different format than in `stats::dnorm()` or `stats::rnorm`,
#' which require a standard deviation.
#'
#' @param log \[`logical(1)`\]\cr
#' For `dmvnorm()`: Return the logarithm of the density value?
#'
#' For `rmvnorm()`: Draw from a log-normal distribution?
#'
#' @param n \[`integer(1)`\]\cr
#' An \code{integer}, the number of requested samples.
#'
#' @return
#' For `dmvnorm()`: The density value.
#'
#' For `rmvnorm()`: If \code{n = 1} a \code{vector} of length \code{p} (note
#' that it is a column vector for `rmvnorm_cpp()`), else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' x <- c(0, 0)
#' mean <- c(0, 0)
#' Sigma <- diag(2)
#'
#' # compute density
#' dmvnorm(x = x, mean = mean, Sigma = Sigma)
#' dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
#'
#' # sample
#' rmvnorm(n = 3, mean = mean, Sigma = Sigma)
#' rmvnorm(mean = mean, Sigma = Sigma, log = TRUE)

dmvnorm <- function(x, mean, Sigma, log = FALSE) {
  input_check_response(
    check = check_numeric_vector(x, any.missing = FALSE, min.len = 1),
    var_name = "x"
  )
  dim <- length(x)
  if (checkmate::test_atomic_vector(mean, len = 1)) {
    mean <- rep(mean, dim)
  }
  input_check_response(
    check = check_numeric_vector(mean, any.missing = FALSE, len = dim),
    var_name = "mean"
  )
  if (dim == 1 && checkmate::test_atomic_vector(Sigma, len = 1)) {
    Sigma <- as.matrix(Sigma)
  }
  input_check_response(
    check = check_covariance_matrix(Sigma, dim = dim),
    var_name = "Sigma"
  )
  input_check_response(
    check = checkmate::check_flag(log),
    var_name = "log"
  )
  dmvnorm_cpp(x, mean, Sigma, log)
}

#' @rdname dmvnorm
#' @export

rmvnorm <- function(n = 1, mean, Sigma, log = FALSE) {
  input_check_response(
    check = checkmate::check_int(n),
    var_name = "n"
  )
  if (checkmate::test_atomic_vector(Sigma, len = 1)) {
    Sigma <- as.matrix(Sigma)
  }
  input_check_response(
    check = check_covariance_matrix(Sigma),
    var_name = "Sigma"
  )
  dim <- nrow(Sigma)
  if (checkmate::test_atomic_vector(mean, len = 1)) {
    mean <- rep(mean, dim)
  }
  input_check_response(
    check = check_numeric_vector(mean, any.missing = FALSE, len = dim),
    var_name = "mean"
  )
  input_check_response(
    check = checkmate::check_flag(log),
    var_name = "log"
  )
  out <- replicate(n = n, rmvnorm_cpp(mean, Sigma, log), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else if (dim == 1) {
    as.matrix(out)
  } else {
    t(out)
  }
}
