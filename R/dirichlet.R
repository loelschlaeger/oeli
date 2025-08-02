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
#' The number of samples.
#'
#' @return
#' For `ddirichlet()`: The density value.
#'
#' For `rdirichlet()`: If \code{n = 1} a \code{vector} of length \code{p}, else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
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
  checkmate::assert_numeric(x, lower = 0, upper = 1, any.missing = FALSE)
  stopifnot("'x' must sum up to 1" = sum(x) == 1)
  checkmate::assert_numeric(concentration, lower = 0, any.missing = FALSE, len = length(x))
  checkmate::assert_flag(log)
  ddirichlet_cpp(x, concentration, log)
}

#' @rdname ddirichlet
#' @export

rdirichlet <- function(n = 1, concentration) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_vector(concentration, strict = TRUE)
  checkmate::assert_numeric(concentration, any.missing = FALSE, lower = 0)
  dim <- length(concentration)
  out <- replicate(n = n, rdirichlet_cpp(concentration), simplify = TRUE)
  if (n == 1) {
    drop(out)
  } else if (dim == 1) {
    as.matrix(out)
  } else {
    t(out)
  }
}
