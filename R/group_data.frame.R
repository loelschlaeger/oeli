#' Grouping of a \code{data.frame}
#'
#' @description
#' This function groups a \code{data.frame} according to values of one column.
#'
#' @param df \[`data.frame`\]\cr
#' A \code{data.frame}.
#'
#' @param by \[`character(1)`\]\cr
#' The name of a column of \code{df} to group by.
#'
#' @param keep_by \[`logical(1)`\]\cr
#' Keep the grouping column \code{by}?
#'
#' @return
#' A \code{list} of \code{data.frame}s, subsets of \code{df}.
#'
#' @keywords transformation
#' @family data.frame helpers
#' @export
#'
#' @examples
#' df <- data.frame("label" = c("A", "B"), "number" = 1:10)
#' group_data.frame(df = df, by = "label")
#' group_data.frame(df = df, by = "label", keep_by = FALSE)

group_data.frame <- function(df, by, keep_by = TRUE) {
  checkmate::assert_data_frame(df)
  checkmate::assert_names(by, subset.of = names(df))
  checkmate::assert_flag(keep_by)
  lapply(
    split(df, as.factor(df[, by])),
    delete_columns_data.frame,
    column_names = if (keep_by) character() else by
  )
}
