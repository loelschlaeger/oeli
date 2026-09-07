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
#' @details
#' `rtnorm()` draws by the rejection methods of Robert (1995), and `rttnorm()`
#' inverts the distribution function of the truncated tail, so that both
#' remain accurate when a truncation point lies far in the tail.
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
#' @param point,lower,upper \[`numeric(1)`\]\cr
#' The truncation point.
#'
#' @param above \[`logical(1)`\]\cr
#' Truncate from above? Else, from below.
#'
#' @param log \[`logical(1)`\]\cr
#' For `dtnorm()` and `dttnorm()`, return the logarithm of the density value?
#'
#' For `rtnorm()` and `rttnorm()`, return the exponential of the draw, which
#' is a draw from the truncated log-normal distribution?
#'
#' @param n \[`integer(1)`\]\cr
#' The number of requested samples.
#'
#' @return
#' For `dtnorm()` and `dttnorm()`: The density value.
#'
#' For `rtnorm()` and `rttnorm()`: A `numeric` of length `n` with the random
#' draws.
#'
#' @references
#' Robert, C. P. (1995). Simulation of truncated normal variables. Statistics
#' and Computing, 5(2), 121-125.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' # compute density
#' dtnorm(x = 1, mean = 0, sd = 1, point = 0, above = FALSE)
#' dttnorm(x = 0, mean = 0, sd = 1, lower = -1, upper = 1, log = TRUE)
#'
#' # sample
#' rtnorm(n = 3, mean = 0, sd = 1, point = 0, above = FALSE)
#' rttnorm(mean = 0, sd = 1, lower = -1, upper = 1)

dtnorm <- function(x, mean, sd, point, above, log = FALSE) {
  input_check_response(checkmate::check_number(x), "x")
  input_check_response(checkmate::check_number(mean), "mean")
  input_check_response(checkmate::check_number(sd, lower = 0), "sd")
  input_check_response(checkmate::check_number(point), "point")
  input_check_response(checkmate::check_flag(above), "above")
  input_check_response(checkmate::check_flag(log), "log")
  dtnorm_cpp(x, mean, sd, point, above, log)
}

#' @rdname dtnorm
#' @export

dttnorm <- function(x, mean, sd, lower, upper, log = FALSE) {
  input_check_response(checkmate::check_number(x), "x")
  input_check_response(checkmate::check_number(mean), "mean")
  input_check_response(checkmate::check_number(sd, lower = 0), "sd")
  input_check_response(checkmate::check_number(lower), "lower")
  input_check_response(checkmate::check_number(upper, lower = lower), "upper")
  input_check_response(checkmate::check_flag(log), "log")
  dttnorm_cpp(x, mean, sd, lower, upper, log)
}

#' @rdname dtnorm
#' @export

rtnorm <- function(n = 1, mean, sd, point, above, log = FALSE) {
  input_check_response(checkmate::check_int(n, lower = 1), "n")
  input_check_response(checkmate::check_number(mean), "mean")
  input_check_response(checkmate::check_number(sd, lower = 0), "sd")
  input_check_response(checkmate::check_number(point), "point")
  input_check_response(checkmate::check_flag(above), "above")
  input_check_response(checkmate::check_flag(log), "log")
  vapply(seq_len(n), function(i) {
    rtnorm_cpp(mean, sd, point, above, log)
  }, numeric(1))
}

#' @rdname dtnorm
#' @export

rttnorm <- function(n = 1, mean, sd, lower, upper, log = FALSE) {
  input_check_response(checkmate::check_int(n, lower = 1), "n")
  input_check_response(checkmate::check_number(mean), "mean")
  input_check_response(checkmate::check_number(sd, lower = 0), "sd")
  input_check_response(checkmate::check_number(lower), "lower")
  input_check_response(checkmate::check_number(upper, lower = lower), "upper")
  input_check_response(checkmate::check_flag(log), "log")
  vapply(seq_len(n), function(i) {
    rttnorm_cpp(mean, sd, lower, upper, log)
  }, numeric(1))
}
