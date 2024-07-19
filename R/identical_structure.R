#' Check if two objects have identical structure
#'
#' @description
#' This function determines whether two objects have the same structure,
#' - which includes the \code{\link[base]{mode}}, \code{\link[base]{class}} and
#'   dimension
#' - but does *not* include concrete values or attributes.
#'
#' @param x,y \[`any`\]\cr
#' Two objects.
#'
#' @return
#' Either \code{TRUE} if \code{x} and \code{y} have the same structure, and
#' \code{FALSE}, else.
#'
#' @keywords validation
#' @family package helpers
#' @export
#'
#' @references
#' Inspired by \url{https://stackoverflow.com/a/45548885/15157768}.
#
#' @examples
#' identical_structure(integer(1), 1L)
#' identical_structure(diag(2), matrix(rnorm(4), 2, 2))
#' identical_structure(diag(2), data.frame(diag(2)))

identical_structure <- function(x, y) {
  compare_type <- function(x, y) {
    if (length(x) == length(y)) {
      all(mapply(x, y,
        FUN = function(x, y) {
          if (is.list(x) && is.list(y)) {
            all(compare_type(x, y))
          } else if (is.list(x) == is.list(y)) {
            identical(x, y)
          } else {
            FALSE
          }
        }
      ))
    } else {
      FALSE
    }
  }
  if (is.list(x) && is.list(y)) {
    compare_type(
      rapply(x, function(values) c(mode(values), length(values)), how = "list"),
      rapply(y, function(values) c(mode(values), length(values)), how = "list")
    )
  } else {
    if (!identical(class(x), class(y))) {
      FALSE
    } else {
      compare_type(
        if (is.null(dim(x))) length(x) else dim(x),
        if (is.null(dim(y))) length(y) else dim(y)
      )
    }
  }
}
