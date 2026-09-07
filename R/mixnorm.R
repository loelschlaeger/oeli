#' Mixture of normal distributions
#'
#' @description
#' The function `dmixnorm()` computes the density of a mixture of multivariate
#' normal distribution.
#'
#' The function `pmixnorm()` computes the cumulative distribution function of a
#' mixture of multivariate normal distribution.
#'
#' The function `rmixnorm()` samples from a mixture of multivariate normal
#' distribution.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' The univariate normal mixture is available as the special case `p = 1`.
#'
#' @details
#' `pmixnorm()` is based on [pmvnorm()], which is exact for `p <= 3` and
#' approximates the probability by the method selected in `method` otherwise.
#'
#' @param x \[`numeric(p)`\]\cr
#' A quantile vector of length `p`, where `p` is the dimension.
#'
#' @param mean \[`matrix(nrow = p, ncol = c)`\]\cr
#' The mean vectors for each component in columns.
#'
#' @param Sigma \[`matrix(nrow = p^2, ncol = c)`\]\cr
#' The vectorized covariance matrices for each component in columns.
#'
#' @param proportions \[`numeric(c)`\]\cr
#' The non-negative mixing proportions for each components.
#'
#' If proportions do not sum to unity, they are rescaled to do so.
#'
#' @param log \[`logical(1)`\]\cr
#' For `dmixnorm()`, return the logarithm of the density value?
#'
#' For `rmixnorm()`, return the exponential of the draw, which is a draw from
#' the mixture of log-normal distributions?
#'
#' @inheritParams dmvnorm
#'
#' @return
#' For `dmixnorm()`: The density value.
#'
#' For `pmixnorm()`: The value of the distribution function or the rectangle
#' probability.
#'
#' For `rmixnorm()`: If \code{n = 1} a \code{vector} of length \code{p} (note
#' that it is a column vector for `rmixnorm_cpp()`), else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' x <- c(0, 0)
#' mean <- matrix(c(-1, -1, 0, 0), ncol = 2)
#' Sigma <- matrix(c(diag(2), diag(2)), ncol = 2)
#' proportions <- c(0.7, 0.3)
#'
#' # compute density
#' dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
#' dmixnorm(
#'   x = x, mean = mean, Sigma = Sigma, proportions = proportions, log = TRUE
#' )
#'
#' # compute CDF
#' pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
#'
#' # compute rectangle probability
#' pmixnorm(
#'   x = x, mean = mean, Sigma = Sigma, proportions = proportions, lower = -1
#' )
#'
#' # sample
#' rmixnorm(n = 3, mean = mean, Sigma = Sigma, proportions = proportions)

dmixnorm <- function(x, mean, Sigma, proportions, log = FALSE) {
  input_check_response(
    check = check_numeric_vector(x, any.missing = FALSE, min.len = 1),
    var_name = "x"
  )
  dim <- length(x)
  input_check_response(
    check = checkmate::check_matrix(
      mean, mode = "numeric", any.missing = FALSE, nrows = dim, min.cols = 1
    ),
    var_name = "mean"
  )
  ncomp <- ncol(mean)
  input_check_response(
    check = checkmate::check_matrix(
      Sigma, mode = "numeric", any.missing = FALSE, nrows = dim^2, ncols = ncomp
    ),
    var_name = "Sigma"
  )
  for (c in seq_len(ncomp)) {
    input_check_response(
      check = check_covariance_matrix(
        matrix(Sigma[, c], nrow = dim, ncol = dim), dim = dim
      ),
      var_name = paste0("matrix(Sigma[, ", c, "], nrow = dim, ncol = dim)")
    )
  }
  input_check_response(
    check = check_numeric_vector(
      proportions, lower = 0, finite = TRUE, any.missing = FALSE, len = ncomp
    ),
    var_name = "proportions"
  )
  input_check_response(
    check = checkmate::check_flag(log),
    var_name = "log"
  )
  dmixnorm_cpp(x, mean, Sigma, proportions, log)
}

#' @rdname dmixnorm
#' @export

pmixnorm <- function(
    x, mean, Sigma, proportions, abseps = 1e-3, lower = NULL, method = "genz",
    draws = 500
) {
  input_check_response(
    check = check_numeric_vector(x, any.missing = FALSE, min.len = 1),
    var_name = "x"
  )
  dim <- length(x)
  input_check_response(
    check = checkmate::check_matrix(
      mean, mode = "numeric", any.missing = FALSE, nrows = dim, min.cols = 1
    ),
    var_name = "mean"
  )
  ncomp <- ncol(mean)
  input_check_response(
    check = checkmate::check_matrix(
      Sigma, mode = "numeric", any.missing = FALSE, nrows = dim^2, ncols = ncomp
    ),
    var_name = "Sigma"
  )
  for (c in seq_len(ncomp)) {
    input_check_response(
      check = check_covariance_matrix(
        matrix(Sigma[, c], nrow = dim, ncol = dim), dim = dim
      ),
      var_name = paste0("matrix(Sigma[, ", c, "], nrow = dim, ncol = dim)")
    )
  }
  input_check_response(
    check = check_numeric_vector(
      proportions, lower = 0, finite = TRUE, any.missing = FALSE, len = ncomp
    ),
    var_name = "proportions"
  )
  input_check_response(
    check = checkmate::check_number(abseps, lower = 0, finite = TRUE),
    var_name = "abseps"
  )
  if (checkmate::test_atomic_vector(lower, len = 1)) {
    lower <- rep(lower, dim)
  }
  input_check_response(
    check = check_numeric_vector(
      lower, any.missing = FALSE, len = dim, null.ok = TRUE
    ),
    var_name = "lower"
  )
  input_check_response(
    check = checkmate::check_choice(method, c("genz", "ghk")),
    var_name = "method"
  )
  input_check_response(
    check = checkmate::check_int(draws, lower = 2),
    var_name = "draws"
  )
  pmixnorm_cpp(x, mean, Sigma, proportions, abseps, lower, method, draws)
}

#' @rdname dmixnorm
#' @export

rmixnorm <- function(n = 1, mean, Sigma, proportions, log = FALSE) {
  input_check_response(
    check = checkmate::check_int(n, lower = 1),
    var_name = "n"
  )
  input_check_response(
    check = checkmate::check_matrix(
      mean, mode = "numeric", any.missing = FALSE, min.rows = 1, min.cols = 1
    ),
    var_name = "mean"
  )
  dim <- nrow(mean)
  ncomp <- ncol(mean)
  input_check_response(
    check = checkmate::check_matrix(
      Sigma, mode = "numeric", any.missing = FALSE, nrows = dim^2, ncols = ncomp
    ),
    var_name = "Sigma"
  )
  for (c in seq_len(ncomp)) {
    input_check_response(
      check = check_covariance_matrix(
        matrix(Sigma[, c], nrow = dim, ncol = dim), dim = dim
      ),
      var_name = paste0("matrix(Sigma[, ", c, "], nrow = dim, ncol = dim)")
    )
  }
  input_check_response(
    check = check_numeric_vector(
      proportions, lower = 0, finite = TRUE, any.missing = FALSE, len = ncomp
    ),
    var_name = "proportions"
  )
  input_check_response(
    check = checkmate::check_flag(log),
    var_name = "log"
  )
  sample <- replicate(
    n = n, rmixnorm_cpp(mean, Sigma, proportions, log), simplify = TRUE
  )
  if (n == 1) {
    drop(sample)
  } else if (dim == 1) {
    as.matrix(sample)
  } else {
    t(sample)
  }
}
