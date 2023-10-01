#' Removing an index
#'
#' @description
#' This function removes a value from an index vector and optionally shifts the
#' indices up.
#'
#' @param x
#' An \code{integer} (vector).
#' @param index
#' An \code{integer}.
#' @param replace
#' Either \code{TRUE} the shift the indices up, or \code{FALSE} (default) else.
#'
#' @return
#' An \code{integer} (vector).
#'
#' @export

remove_index <- function(x, index, replace = FALSE) {
  checkmate::assert_integerish(x)
  checkmate::assert_int(index)
  checkmate::assert_logical(replace, len = 1)
  x <- x[!(x %in% index)]
  if (replace) x[x >= index] <- x[x >= index] - 1
  return(x)
}
