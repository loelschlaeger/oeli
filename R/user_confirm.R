#' User confirmation
#'
#' @description
#' This function asks in an interactive question a binary question.
#'
#' @param question \[`character(1)`\]\cr
#' The binary question to ask. It should end with a question mark.
#'
#' @param default \[`logical(1)`\]\cr
#' The default decision.
#'
#' @return
#' Either \code{TRUE} or \code{FALSE}.
#'
#' @keywords validation
#' @family package helpers
#' @export

user_confirm <- function(question = "Question?", default = FALSE) {
  checkmate::assert_flag(default)
  if (!interactive()) {
    return(default)
  }
  checkmate::assert_string(question)
  repeat {
    selection <- ifelse(default, "[Y/n]", "[y/N]")
    cli::cli_inform("{question} {selection}:")
    response <- tryCatch(
      tolower(trimws(readline())),
      interrupt = identity
    )
    if (inherits(response, "interrupt")) {
      stop()
    }
    if (!nzchar(response)) {
      return(default)
    }
    if (response %in% c("y", "yes")) {
      return(TRUE)
    }
    if (response %in% c("n", "no")) {
      return(FALSE)
    }
    cli::cli_inform("Please enter 'y' or 'n'.")
  }
}
