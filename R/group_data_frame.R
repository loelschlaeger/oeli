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
