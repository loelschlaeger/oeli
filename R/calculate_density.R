#' @inherit ddirichlet_cpp title description return
#' @inheritParams ddirichlet_cpp
#'
#' @examples
#' x <- c(0.5, 0.3, 0.2)
#' concentration <- 1:3
#' ddirichlet(x = x, concentration = concentration)
#' ddirichlet(x = x, concentration = concentration, log = TRUE)
#'
#' @export

ddirichlet <- function(x, concentration, log = FALSE) {
  checkmate::assert_numeric(x, lower = 0, upper = 1, any.missing = FALSE)
  stopifnot("'x' must sum up to 1" = sum(x) == 1)
  checkmate::assert_numeric(concentration, lower = 0, any.missing = FALSE, len = length(x))
  checkmate::assert_flag(log)
  ddirichlet_cpp(x, concentration, log)
}

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

#' @inherit dwishart_cpp title description return
#' @inheritParams dwishart_cpp
#'
#' @examples
#' x <- diag(2)
#' df <- 4
#' scale <- diag(2)
#' dwishart(x = x, df = df, scale = scale)
#' dwishart(x = x, df = df, scale = scale, log = TRUE)
#' dwishart(x = x, df = df, scale = scale, inv = TRUE)
#'
#' @export

dwishart <- function(x, df, scale, log = FALSE, inv = FALSE) {
  assert_covariance_matrix(x)
  checkmate::assert_int(df, lower = nrow(x))
  assert_covariance_matrix(scale, dim = nrow(x))
  checkmate::assert_flag(log)
  checkmate::assert_flag(inv)
  dwishart_cpp(x, df, scale, log, inv)
}
