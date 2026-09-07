#' Wishart distribution
#'
#' @description
#' The function `dwishart()` computes the density of a Wishart distribution.
#'
#' The function `rwishart()` samples from a Wishart distribution.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' @param x \[`matrix()`\]\cr
#' A covariance matrix of dimension `p`.
#'
#' @param df \[`numeric(1)`\]\cr
#' The degrees of freedom, at least `p`.
#'
#' @param scale \[`matrix()`\]\cr
#' The scale covariance matrix of dimension `p`.
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the density value?
#'
#' @param inv \[`logical(1)`\]\cr
#' Use this inverse Wishart distribution?
#'
#' @param n \[`integer(1)`\]\cr
#' The number of requested samples.
#'
#' @return
#' For `dwishart()`: The density value.
#'
#' For `rwishart()`: If `n = 1` a `matrix` of dimension `p` times `p`, else
#' an `array` of dimension `p` times `p` times `n` with the draws as slices.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' x <- diag(2)
#' df <- 6
#' scale <- matrix(c(1, -0.3, -0.3, 0.8), ncol = 2)
#'
#' # compute density
#' dwishart(x = x, df = df, scale = scale)
#' dwishart(x = x, df = df, scale = scale, log = TRUE)
#' dwishart(x = x, df = df, scale = scale, inv = TRUE)
#'
#' # sample
#' rwishart(df = df, scale = scale)
#' rwishart(df = df, scale = scale, inv = TRUE)
#'
#' # expectation of Wishart is df * scale
#' apply(rwishart(n = 100, df = df, scale = scale), 1:2, mean)
#' df * scale
#'
#' # expectation of inverse Wishart is scale / (df - p - 1)
#' apply(rwishart(n = 100, df = df, scale = scale, inv = TRUE), 1:2, mean)
#' scale / (df - 2 - 1)

dwishart <- function(x, df, scale, log = FALSE, inv = FALSE) {
  input_check_response(check_covariance_matrix(x), "x")
  input_check_response(checkmate::check_number(df, lower = nrow(x)), "df")
  input_check_response(check_covariance_matrix(scale, dim = nrow(x)), "scale")
  input_check_response(checkmate::check_flag(log), "log")
  input_check_response(checkmate::check_flag(inv), "inv")
  dwishart_cpp(x, df, scale, log, inv)
}

#' @rdname dwishart
#' @export

rwishart <- function(n = 1, df, scale, inv = FALSE) {
  input_check_response(checkmate::check_int(n, lower = 1), "n")
  input_check_response(check_covariance_matrix(scale), "scale")
  input_check_response(checkmate::check_number(df, lower = nrow(scale)), "df")
  input_check_response(checkmate::check_flag(inv), "inv")
  draws <- replicate(n, rwishart_cpp(df, scale, inv), simplify = FALSE)
  if (n == 1) draws[[1]] else simplify2array(draws)
}
