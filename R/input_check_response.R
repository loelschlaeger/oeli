


input_check_response <- function(
    check, var_name, error = TRUE, prefix = "Input {var_name} is bad:"
  ) {
  if (!isTRUE(check)) {
    if (isTRUE(error)) {
      cli::cli_abort(paste(prefix, "{check}"), call = NULL)
    } else {
      FALSE
    }
  } else {
    TRUE
  }
}
