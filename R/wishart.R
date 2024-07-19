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
#' @param df \[`integer()`\]\cr
#' The degrees of freedom greater of equal `p`.
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
#' @return
#' For `dwishart()`: The density value.
#'
#' For `rwishart()`: A `matrix`, the random draw.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' x <- diag(2)
#' df <- 4
#' scale <- diag(2)
#'
#' # compute density
#' dwishart(x = x, df = df, scale = scale)
#' dwishart(x = x, df = df, scale = scale, log = TRUE)
#' dwishart(x = x, df = df, scale = scale, inv = TRUE)
#'
#' # sample
#' rwishart(df = df, scale = scale)
#' rwishart(df = df, scale = scale, inv = TRUE)

dwishart <- function(x, df, scale, log = FALSE, inv = FALSE) {
  assert_covariance_matrix(x)
  checkmate::assert_int(df, lower = nrow(x))
  assert_covariance_matrix(scale, dim = nrow(x))
  checkmate::assert_flag(log)
  checkmate::assert_flag(inv)
  dwishart_cpp(x, df, scale, log, inv)
}

#' @rdname dwishart
#' @export

rwishart <- function(df, scale, inv = FALSE) {
  assert_covariance_matrix(scale)
  checkmate::assert_int(df, lower = nrow(scale))
  checkmate::assert_flag(inv)
  rwishart_cpp(df, scale, inv)
}
