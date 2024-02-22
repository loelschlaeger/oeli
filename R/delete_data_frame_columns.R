#' Deleting \code{data.frame} columns
#'
#' @description
#' This function deletes columns of a \code{data.frame} by name.
#'
#' @param df
#' A \code{data.frame}.
#' @param column_names
#' A \code{character} (\code{vector}), the name(s) of a column of \code{df} to group by.
#'
#' @return
#' The input \code{df} without the columns defined by \code{column_names}.
#'
#' @examples
#' df <- data.frame("label" = c("A", "B"), "number" = 1:10)
#' delete_data_frame_columns(df = df, column_names = "label")
#' delete_data_frame_columns(df = df, column_names = "number")
#' delete_data_frame_columns(df = df, column_names = c("label", "number"))
#'
#' @export

delete_data_frame_columns <- function(df, column_names) {
  checkmate::assert_data_frame(df)
  checkmate::assert_names(column_names, subset.of = names(df))
  df[column_names] <- NULL
  return(df)
}
