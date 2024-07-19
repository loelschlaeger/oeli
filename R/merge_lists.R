#' Merge named lists
#'
#' @description
#' This function merges \code{list}s based on their element names. Elements are
#' only included in the final output \code{list}, if no former \code{list} has
#' contributed an element with the same name.
#'
#' @param ...
#' One or more named \code{list}(s).
#'
#' @return
#' A \code{list}.
#'
#' @export
#' @keywords transformation
#' @family list helpers
#'
#' @examples
#' merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4, "d" = NULL))

merge_lists <- function(...) {
  inputs <- list(...)
  lapply(inputs, function(input) checkmate::assert_list(input, names = "unique"))
  final <- list()
  for (input in inputs) {
    for (element in names(input)) {
      if (!element %in% names(final)) {
        final[element] <- input[element]
      }
    }
  }
  return(final)
}
