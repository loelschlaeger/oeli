#' Provide information about occurrences
#'
#' @description
#' This function provides verbose information about absolute or relative
#' element occurrences in `data.frame` columns.
#'
#' @param x \[`data.frame`\]\cr
#' The object to check for occurrences.
#'
#' @param relative \[`logical(1)` \]\cr
#' The number of rows or columns to be printed, greater or equal `2`.
#'
#' @param named \[`logical(1)` \]\cr
#' Prepend column names of `x` (if not `NA`)?
#'
#' @return
#' A `character()`.
#'
#' @export
#' @keywords packaging
#' @family data.frame helpers
#'
#' @examples
#' occurrence_info(datasets::warpbreaks, relative = FALSE, named = TRUE)

occurrence_info <- function(x, relative = FALSE, named = FALSE) {

  ### input checks
  input_check_response(
    check = checkmate::check_data_frame(x),
    var_name = "x"
  )
  input_check_response(
    check = checkmate::check_flag(relative),
    var_name = "relative"
  )
  input_check_response(
    check = checkmate::check_flag(named),
    var_name = "named"
  )

  ### extract unique values
  r <- nrow(x)
  n <- ncol(x)
  m <- colnames(x)
  out <- character(n)
  table_values <- sapply(x, table, useNA = "ifany")
  for (i in seq_len(n)) {
    tab <- table_values[[i]] |> sort(decreasing = TRUE)
    occ <- if (relative) {
      paste0(ceiling(tab / r * 100), "% ", names(tab), collapse = ", ")
    } else {
      paste0(tab, "x ", names(tab), collapse = ", ")
    }
    out[i] <- if (named & checkmate::test_string(m[i])) {
      paste0(m[i], ": ", occ)
    } else {
      occ
    }
  }
  return(out)
}
