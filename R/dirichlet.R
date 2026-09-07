#' Dirichlet distribution
#'
#' @description
#' The function `ddirichlet()` computes the density of a Dirichlet distribution.
#'
#' The function `rdirichlet()` samples from a Dirichlet distribution.
#'
#' The functions with suffix `_cpp` perform no input checks, hence are faster.
#'
#' @param x \[`numeric()`\]\cr
#' A probability vector.
#'
#' @param concentration \[`numeric()`\]\cr
#' A concentration vector of the same length as `x`.
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the density value?
#'
#' @param n \[`integer(1)`\]\cr
#' The number of requested samples.
#'
#' @return
#' For `ddirichlet()`: The density value.
#'
#' For `rdirichlet()`: If `n = 1` a `vector` of length `p`, else a `matrix` of
#' dimension `n` times `p` with samples as rows.
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' x <- c(0.5, 0.3, 0.2)
#' concentration <- 1:3
#'
#' # compute density
#' ddirichlet(x = x, concentration = concentration)
#' ddirichlet(x = x, concentration = concentration, log = TRUE)
#'
#' # sample
#' rdirichlet(concentration = 1:3)
#' rdirichlet(n = 4, concentration = 1:2)

ddirichlet <- function(x, concentration, log = FALSE) {
  input_check_response(check_probability_vector(x), "x")
  input_check_response(
    check_numeric_vector(
      concentration, lower = 0, any.missing = FALSE, len = length(x)
    ),
    "concentration"
  )
  input_check_response(checkmate::check_flag(log), "log")
  ddirichlet_cpp(x, concentration, log)
}

#' @rdname ddirichlet
#' @export

rdirichlet <- function(n = 1, concentration) {
  input_check_response(checkmate::check_int(n, lower = 1), "n")
  input_check_response(
    check_numeric_vector(concentration, lower = 0, any.missing = FALSE),
    "concentration"
  )
  dim <- length(concentration)
  draws <- replicate(n = n, rdirichlet_cpp(concentration), simplify = TRUE)
  if (n == 1) {
    drop(draws)
  } else if (dim == 1) {
    as.matrix(draws)
  } else {
    t(draws)
  }
}
