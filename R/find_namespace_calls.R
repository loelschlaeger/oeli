#' Namespace calls
#'
#' @description
#' This function searches for namespace calls in `.R` files, i.e., code lines of
#' the format `<package name>::<function name>`.
#'
#' @param path \[`character(1)`\]\cr
#' The path name to a folder. All `.R` files in this folder and sub-directories
#' will be searched.
#'
#' @param triple_colon \[`logical(1)`\]\cr
#' Also search for `:::`?
#'
#' @param as_list \[`logical(1)`\]\cr
#' Simplify the output into a `list` of unique function names per package?
#'
#' @return
#' A `data.frame`. If `as_list = TRUE`, a `list`.
#'
#' @keywords packaging
#' @family package helpers
#' @export
#'
#' @examples
#' \dontrun{
#' find_namespace_calls()
#' find_namespace_calls(as_list = TRUE)
#' }

find_namespace_calls <- function(
    path = "R", triple_colon = FALSE, as_list = FALSE
  ) {

  ### input checks
  input_check_response(
    check = checkmate::check_directory_exists(path, access = "r"),
    var_name = "path"
  )
  input_check_response(
    check = checkmate::check_flag(triple_colon),
    var_name = "triple_colon"
  )
  input_check_response(
    check = checkmate::check_flag(as_list),
    var_name = "as_list"
  )

  ### list .R files under path
  pattern <- "\\.R$"
  r_files <- list.files(
    path = path, pattern = pattern, full.names = TRUE, recursive = TRUE
  )

  ### regular expressions to match :: and ::: calls followed by a character that
  ### is NOT part of a valid function name
  pattern_double <- "\\b([a-zA-Z0-9\\.]+)::([a-zA-Z0-9_\\.]+)(?![a-zA-Z0-9_\\.])"
  pattern_triple <- "\\b([a-zA-Z0-9\\.]+):::+([a-zA-Z0-9_\\.]+)(?![a-zA-Z0-9_\\.])"
  pattern <- if (triple_colon) {
    paste0("(", pattern_double, "|", pattern_triple, ")")
  } else {
    pattern_double
  }

  ### search
  matches <- list()

  for (file in r_files) {
    lines <- readLines(file, warn = FALSE)

    for (i in seq_along(lines)) {
      line <- lines[i]
      found <- gregexpr(pattern, line, perl = TRUE)[[1]]

      if (found[1] != -1) {
        matched_texts <- regmatches(line, gregexpr(pattern, line, perl = TRUE))[[1]]
        for (match in matched_texts) {
          if (grepl(":::", match)) {
            pkg <- sub("^([a-zA-Z0-9\\.]+):::+.*", "\\1", match)
            fun <- sub("^[a-zA-Z0-9\\.]+:::+([a-zA-Z0-9_\\.]+).*", "\\1", match)
            full_call <- paste0(pkg, "::: ", fun)
          } else {
            pkg <- sub("^([a-zA-Z0-9\\.]+)::.*", "\\1", match)
            fun <- sub("^[a-zA-Z0-9\\.]+::([a-zA-Z0-9_\\.]+).*", "\\1", match)
            full_call <- paste0(pkg, "::", fun)
          }
          matches <- append(matches, list(list(
            file = file,
            line_number = i,
            call = full_call,
            pkg = pkg,
            fun = fun
          )))
        }
      }
    }
  }

  ### prepare output
  df <- if (length(matches) == 0) {
    data.frame(file = character(0), line_number = integer(0), call = character(0))
  } else {
    do.call(rbind, lapply(matches, as.data.frame, stringsAsFactors = FALSE))
  }
  if (as_list) {
    if (length(matches) == 0) {
      return(list())
    }
    return(lapply(split(df$fun, df$pkg), unique))
  }
  return(df)
}

