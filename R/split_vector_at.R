#' Split a vector at positions
#'
#' @description
#' This function splits a vector at specific positions.
#'
#' @param x \[atomic()`\]\cr
#' A vector of elements.
#'
#' @param at \[`integer()`\]\cr
#' Index position(s) just before to split.
#'
#' For example, \code{at = n} splits before the \code{n}th element of \code{x}.
#'
#' @return
#' A \code{list}.
#'
#' @keywords transformation
#' @family vector helpers
#' @export
#'
#' @references
#' Based on https://stackoverflow.com/a/19274414.
#'
#' @examples
#' x <- 1:10
#' split_vector_at(x, c(2, 3, 5, 7))

split_vector_at <- function(x, at) {
  input_check_response(
    check = checkmate::check_atomic_vector(x),
    var_name = "x"
  )
  input_check_response(
    check = checkmate::check_integerish(
      at, lower = 1, upper = length(x), unique = TRUE, any.missing = FALSE
    ),
    var_name = "at"
  )
  if (length(x) %in% c(0, 1)) {
    return(list(x))
  }
  at <- unique(c(1L, at, length(x) + 1L))
  Map(
    function(x, i, j) x[i:j], list(x),
    utils::head(at, -1L), utils::tail(at, -1L) - 1L
  )
}
