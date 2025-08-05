#' Multivariate normal distribution
#'
#' @description
#' The function `dmvnorm()` computes the density of a multivariate normal
#' distribution.
#'
#' The function `pmvnorm()` computes the cumulative distribution function of a
#' multivariate normal distribution.
#'
#' The function `rmvnorm()` samples from a multivariate normal distribution.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' The univariate normal distribution is available as the special case `p = 1`.
#'
#' @details
#' `pmvnorm()` just calls `mvtnorm::pmvnorm` with the randomized
#' Quasi-Monte-Carlo procedure by Genz and Bretz. The argument `abseps` controls
#' the accuracy of the Gaussian integral approximation.
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
#' For `rmvnorm()`, arbitrary dimensions (i.e., full rows and corresponding
#' columns) of `Sigma` can be `0`.
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
#' @param abseps \[`numeric(1)`\]\cr
#' The absolute error tolerance.
#'
#' @param n \[`integer(1)`\]\cr
#' The number of requested samples.
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
#' # compute CDF
#' pmvnorm(x = x, mean = mean, Sigma = Sigma)
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

pmvnorm <- function(x, mean, Sigma, abseps = 1e-3) {
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
    check = checkmate::check_number(abseps, lower = 0, finite = TRUE),
    var_name = "abseps"
  )
  pmvnorm_cpp(x, mean, Sigma, abseps)
}

#' @rdname dmvnorm
#' @export

rmvnorm <- function(n = 1, mean, Sigma, log = FALSE) {

  ### input checks
  input_check_response(
    check = checkmate::check_int(n),
    var_name = "n"
  )
  if (checkmate::test_atomic_vector(Sigma, len = 1)) {
    Sigma <- as.matrix(Sigma)
  }
  input_check_response(
    check = checkmate::check_matrix(Sigma, mode = "numeric"),
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

  ### zero dimensions case
  zero_dims <- which(
    apply(Sigma, 1, function(row) all(row == 0)) &
    apply(Sigma, 2, function(col) all(col == 0))
  )
  if (length(zero_dims) == dim) {
    ### the case where Sigma = 0 is handeled in rmvnorm_cpp
    zero_dims <- integer()
  }
  if (length(zero_dims) > 0) {
    mean_det <- mean[zero_dims]
    mean <- mean[-zero_dims]
    Sigma <- Sigma[-zero_dims, -zero_dims, drop = FALSE]
  }
  input_check_response(
    check = check_covariance_matrix(Sigma),
    var_name = "Sigma"
  )

  ### sample
  sample <- replicate(n = n, rmvnorm_cpp(mean, Sigma, log), simplify = TRUE)
  if (n == 1) {
    if (length(zero_dims) > 0) {
      out <- numeric(dim)
      out[-zero_dims] <- drop(sample)
      out[zero_dims] <- mean_det
      if (log) out[zero_dims] <- exp(out[zero_dims])
    } else {
      out <- drop(sample)
    }
  } else if (dim == 1) {
    out <- as.matrix(sample)
  } else {
    if (length(zero_dims) > 0) {
      out <- matrix(0, nrow = n, ncol = dim)
      out[, -zero_dims] <- t(sample)
      out[, zero_dims] <- matrix(
        mean_det, nrow = n, ncol = length(zero_dims), byrow = TRUE
      )
      if (log) out[, zero_dims] <- exp(out[, zero_dims])
    } else {
      out <- t(sample)
    }
  }

  ### return
  return(out)
}
