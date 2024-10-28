#' Map indices
#'
#' @description
#' This function maps indices from an input vector to corresponding sequences of
#' grouped indices. Each element from the input specifies a group to be mapped
#' from the sequence, determined by the grouping size `n`.
#'
#' @param indices \[`integer()`\]\cr
#' An index vector, where each element specifies a group to be mapped from the
#' sequence.
#'
#' @param n \[`integer`\]\cr
#' The size of each group of consecutive indices.
#'
#' @return
#' An `integer` `vector`, containing the mapped indices according to the
#' specified group size.
#'
#' @details
#' This function is useful when working with indices arranged in fixed-size
#' groups, where each group can be referenced by a single index. For example, if
#' indices are structured in chunks of 3, calling this function with `n = 3`
#' will map the corresponding groups of 3 consecutive indices for the given
#' input indices, see the examples.
#'
#' @export
#' @keywords indexing
#' @family vector helpers
#'
#' @examples
#' # Example: Map indices based on groups of 3
#' map_indices(c(1, 3, 5), 3)

map_indices <- function(indices, n) {
  input_check_response(
    check = checkmate::check_integerish(indices, lower = 1, any.missing = FALSE),
    var_name = "indices"
  )
  input_check_response(
    check = checkmate::check_count(n),
    var_name = "n"
  )
  if (length(indices) == 0 || n == 0) return(integer(0))
  unlist(lapply(indices, function(i) ((i - 1) * n + 1):(i * n)))
}
