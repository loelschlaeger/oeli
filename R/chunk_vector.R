#' Split a vector into chunks
#'
#' @description
#' This function either
#' - splits a vector into \code{n} chunks of equal size (\code{type = 1}),
#' - splits a vector into chunks of size \code{n} (\code{type = 2}).
#'
#' @param x \[atomic()`\]\cr
#' A vector of elements.
#'
#' @param n \[`integer(1)`\]\cr
#' A number smaller or equal \code{length(x)}.
#'
#' @param type \[`1` | `2`\]\cr
#' Either
#'
#' - \code{1} (default) to split \code{x} into \code{n} chunks of equal size,
#' - or \code{2} to split \code{x} into chunks of size \code{n}.
#'
#' @param strict \[`logical(1)`\]\cr
#' Set to \code{TRUE} to fail if \code{length(x)} is not a multiple of \code{n},
#' or \code{FALSE} (default), else.
#'
#' @return
#' A \code{list}.
#'
#' @keywords transformation
#' @family vector helpers
#' @export
#'
#' @examples
#' x <- 1:12
#' chunk_vector(x, n = 3, type = 1)
#' chunk_vector(x, n = 3, type = 2)
#' try(chunk_vector(x, n = 5, strict = TRUE))

chunk_vector <- function(x, n, type = 1, strict = FALSE) {
  input_check_response(
    checkmate::check_atomic_vector(x),
    "x"
  )
  input_check_response(
    checkmate::check_int(n, lower = 1),
    "n"
  )
  input_check_response(
    checkmate::check_choice(type, c(1, 2)),
    "type"
  )
  input_check_response(
    checkmate::check_flag(strict),
    "strict"
  )
  if (strict) {
    input_check_response(
      if (length(x) %% n == 0) TRUE else "Not a multiple of 'length(x)'",
      "n"
    )
  }
  if (type == 1) {
    split(x, cut(seq_along(x), n, labels = FALSE))
  } else {
    split(x, ceiling(seq_along(x) / n))
  }
}
