#' Deleting \code{data.frame} columns
#'
#' @description
#' This function deletes columns of a \code{data.frame} by name.
#'
#' @param df \[`data.frame`\]\cr
#' A \code{data.frame}.
#'
#' @param column_names \[`character()`\]\cr
#' The name(s) of column(s) of \code{df} to delete.
#'
#' @return
#' The input \code{df} without the columns defined by \code{column_names}.
#'
#' @keywords transformation
#' @family data.frame helpers
#' @export
#'
#' @examples
#' df <- data.frame("label" = c("A", "B"), "number" = 1:10)
#' delete_columns_data.frame(df = df, column_names = "label")
#' delete_columns_data.frame(df = df, column_names = "number")
#' delete_columns_data.frame(df = df, column_names = c("label", "number"))

delete_columns_data.frame <- function(df, column_names) {
  input_check_response(
    check = checkmate::check_data_frame(df),
    var_name = "df"
  )
  input_check_response(
    check = checkmate::check_names(column_names, subset.of = names(df)),
    var_name = "column_names"
  )
  df[column_names] <- NULL
  return(df)
}
