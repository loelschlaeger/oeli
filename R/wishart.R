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
