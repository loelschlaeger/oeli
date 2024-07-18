#' Dirichlet distribution
#'
#' @description
#' This function computes the density of a Dirichlet distribution.
#'
#' @details
#' TODO input checks
#'
#' @param x
#' A \code{numeric}, a weight vector of length \code{p}.
#' Each vector element must be between \code{0} and \code{1}.
#' The sum of the vector elements must be \code{1}.
#' @param concentration
#' A \code{numeric}, the concentration vector of length \code{p}.
#' @param log
#' A \code{logical}, if \code{TRUE} the logarithm of the density value is
#' returned.
#' By default, \code{log = FALSE}.
#' @param n
#' An \code{integer}, the number of samples.
#'
#' @return
#' TODO If \code{n = 1} a \code{vector} of length \code{p}, else
#' a \code{matrix} of dimension \code{n} times \code{p} with samples as rows.
#' A \code{numeric}, the density value.
#'
#' @keywords distribution
#' @family distribution helpers
#' @export
#'
#' @examples
#' x <- c(0.5, 0.3, 0.2)
#' concentration <- 1:3
#' ddirichlet(x = x, concentration = concentration)
#' ddirichlet(x = x, concentration = concentration, log = TRUE)
#'
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
