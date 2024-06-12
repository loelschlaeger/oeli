#' Find the positions of first or last occurrence of unique vector elements
#'
#' @description
#' This function finds the positions of first or last occurrence of unique
#' vector elements.
#'
#' @param x
#' A \code{vector}.
#'
#' @param type
#' Either \code{"first"} for the first or \code{"last"} for the last occurrence.
#'
#' @return
#' An \code{integer} \code{vector}, the positions of the unique vector elements.
#' The ordering corresponds to \code{unique(x)}, i.e., the \eqn{i}-th element in
#' the output is the (first or last) occurrence of the \eqn{i}-th element from
#' \code{unique(x)}.
#'
#' @export
#'
#' @examples
#' x <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' unique(x)
#' vector_occurrence(x, "first")
#' vector_occurrence(x, "last")

vector_occurrence <- function(x, type = "first") {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_choice(type, c("first", "last"))
  if (type == "first") {
    which(!duplicated(x))
  } else {
    length(x) - match(unique(x), rev(x)) + 1L
  }
}
