#' Grouping of a \code{data.frame}
#'
#' @description
#' This function groups a \code{data.frame} according to values of one column.
#'
#' @param df
#' A \code{data.frame}.
#' @param by
#' A \code{character}, the name of a column of \code{df} to group by.
#' @param keep_by
#' Either \code{TRUE} (default) to keep the grouping column \code{by}, or
#' \code{FALSE}, else.
#'
#' @return
#' A \code{list} of \code{data.frame}s, subsets of \code{df}.
#'
#' @examples
#' df <- data.frame("label" = c("A", "B"), "number" = 1:10)
#' group_data_frame(df = df, by = "label")
#' group_data_frame(df = df, by = "label", keep_by = FALSE)
#'
#' @export

group_data_frame <- function(df, by, keep_by = TRUE) {
  checkmate::assert_data_frame(df)
  checkmate::assert_names(by, subset.of = names(df))
  checkmate::assert_flag(keep_by)
  lapply(
    split(df, as.factor(df[, by])),
    delete_data_frame_columns,
    column_names = if (keep_by) character() else by
  )
}

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
