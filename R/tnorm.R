#' Truncated normal distribution
#'
#' @description
#' The function `dtnorm()` computes the density of a truncated normal
#' distribution.
#'
#' The function `rtnorm()` samples from a truncated normal distribution.
#'
#' The function `dttnorm()` and `rttnorm()` compute the density and sample from
#' a two-sided truncated normal distribution, respectively.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' @param x \[`numeric(1)`\]\cr
#' A quantile.
#'
#' @param mean \[`numeric(1)`\]\cr
#' The mean.
#'
#' @param sd \[`numeric(1)`\]\cr
#' The non-negative standard deviation.
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the density value?
#'
#' @param point,lower,upper \[`numeric(1)`\]\cr
#' The truncation point.
#'
#' @param above \[`logical(1)`\]\cr
#' Truncate from above? Else, from below.
#'
#' @return
#' For `dtnorm()` and `dttnorm()`: The density value.
#'
#' For `rtnorm()` and `rttnorm()`: The random draw
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

dtnorm <- function(x, mean, sd, point, above, log = FALSE) {
  checkmate::assert_number(x)
  checkmate::assert_number(mean)
  checkmate::assert_number(sd)
  checkmate::assert_number(point)
  checkmate::assert_flag(above)
  checkmate::assert_flag(log)
  dtnorm_cpp(x, mean, sd, point, above, log)
}

#' @rdname dtnorm
#' @export

dttnorm <- function(x, mean, sd, lower, upper, log = FALSE) {
  checkmate::assert_number(x)
  checkmate::assert_number(mean)
  checkmate::assert_number(sd)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper, lower = lower)
  checkmate::assert_flag(log)
  dttnorm_cpp(x, mean, sd, lower, upper, log)
}

#' @rdname dtnorm
#' @export

rtnorm <- function(mean, sd, point, above, log = FALSE) {
  checkmate::assert_number(mean)
  checkmate::assert_number(sd)
  checkmate::assert_number(point)
  checkmate::assert_flag(above)
  checkmate::assert_flag(log)
  rtnorm_cpp(mean, sd, point, above, log)
}

#' @rdname dtnorm
#' @export

rttnorm <- function(mean, sd, lower, upper, log = FALSE) {
  checkmate::assert_number(mean)
  checkmate::assert_number(sd)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper, lower = lower)
  checkmate::assert_flag(log)
  rttnorm_cpp(mean, sd, lower, upper, log)
}

