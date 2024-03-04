#' Split a vector into chunks
#'
#' @description
#' This function either
#' - splits a vector into \code{n} chunks of equal size (\code{type = 1}),
#' - splits a vector into chunks of size \code{n} (\code{type = 2}).
#'
#' @param x
#' A \code{vector}.
#'
#' @param n
#' An \code{integer} smaller or equal \code{length(x)}.
#'
#' @param type
#' Either \code{1} (default) to split \code{x} into \code{n} chunks of equal
#' size or \code{2} to split \code{x} into chunks of size \code{n}.
#'
#' @param strict
#' Set to \code{TRUE} to fail if \code{length(x)} is not a multiple of \code{n},
#' or \code{FALSE} (default), else.
#'
#' @return
#' A \code{list}.
#'
#' @export
#'
#' @examples
#' x <- 1:12
#' chunk_vector(x, n = 3, type = 1)
#' chunk_vector(x, n = 3, type = 2)
#' try(chunk_vector(x, n = 5, strict = TRUE))

chunk_vector <- function(x, n, type = 1, strict = FALSE) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_choice(type, c(1, 2))
  checkmate::assert_flag(strict)
  if (strict) {
    stopifnot("'n' is not a multiple of 'length(x)'" = length(x) %% n == 0)
  }
  if (type == 1) {
    split(x, cut(seq_along(x), n, labels = FALSE))
  } else {
    split(x, ceiling(seq_along(x) / n))
  }
}
