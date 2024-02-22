#' Check if an argument is a list of lists
#'
#' @description
#' This function checks whether the input is a list that contains list elements.
#'
#' @param x
#' Object to check.
#'
#' @inheritParams checkmate::check_list
#'
#' @return
#' Compare to \code{\link[checkmate]{check_list}}.
#'
#' @export

check_list_of_lists <- function(
    x, len = NULL
) {
  res <- checkmate::check_list(x, len = len)
  if (!isTRUE(res)) {
    return(res)
  }
  for (i in seq_along(x)) {
    res <- checkmate::check_list(x[[i]], len = len)
    if (!isTRUE(res)) {
      return(paste("Check for element", i, "failed:", res))
    }
  }
  return(TRUE)
}

#' @rdname check_list_of_lists
#' @inheritParams checkmate::assert_list
#' @export

assert_list_of_lists <- checkmate::makeAssertionFunction(
  check_list_of_lists
)

#' @rdname check_list_of_lists
#' @inheritParams checkmate::assert_list
#' @export

test_list_of_lists <- checkmate::makeTestFunction(
  check_list_of_lists
)
