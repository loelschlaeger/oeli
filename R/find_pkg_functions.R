#' Find R functions in a package
#'
#' @description
#' This function lists R functions found in an R package. It can inspect the
#' loaded namespace for exported and non-exported functions and, if a package
#' source path is available, scan `.R` files in `R/`.
#'
#' @details
#' `include_namespace = TRUE` inspects the package namespace with
#' `asNamespace()`. This works for installed packages and returns the R
#' functions that are actually available after the package is loaded, including
#' internal functions.
#'
#' `include_source = TRUE` scans the package source files under `R/`. This is
#' useful when `pkg` points to a package source directory, for example while
#' developing a package before it is installed. If both options are `TRUE`, the
#' result is combined and duplicate function names are removed.
#'
#' The `title` column is read from Rd documentation aliases. It is `NA` when no
#' matching Rd documentation entry is available for a function.
#'
#' @param pkg \[`character(1)`\]\cr
#' The name of an installed package or the path to a package source directory.
#'
#' @param include_namespace \[`logical(1)`\]\cr
#' Include functions found in the loaded package namespace?
#'
#' @param include_source \[`logical(1)`\]\cr
#' Include functions found by scanning `.R` source files?
#'
#' @return
#' A `tibble` with one row per found R function and columns `name`,
#' `title`, `exported`, and `signature`.
#'
#' @keywords packaging
#' @family package helpers
#' @export
#'
#' @examples
#' \dontrun{
#' find_pkg_functions("R6")
#' find_pkg_functions(".")
#' }

find_pkg_functions <- function(
    pkg,
    include_namespace = TRUE,
    include_source = TRUE
  ) {

  input_check_response(
    check = checkmate::check_string(pkg, min.chars = 1),
    var_name = "pkg"
  )
  input_check_response(
    check = checkmate::check_flag(include_namespace),
    var_name = "include_namespace"
  )
  input_check_response(
    check = checkmate::check_flag(include_source),
    var_name = "include_source"
  )
  if (!include_namespace && !include_source) {
    cli::cli_abort(
      paste(
        "At least one of {.arg include_namespace} or",
        "{.arg include_source} must be TRUE."
      ),
      call = NULL
    )
  }

  empty <- data.frame(
    name = character(),
    title = character(),
    exported = logical(),
    signature = character(),
    stringsAsFactors = FALSE
  )
  signature <- function(f) {
    formals <- if (is.function(f)) {
      tryCatch(formals(f), error = function(e) NULL)
    } else {
      as.list(f[[2]])
    }
    args <- names(formals)
    if (is.null(args)) {
      args <- character()
    }
    paste0("function(", paste(args, collapse = ", "), ")")
  }
  row <- function(name, title, exported, signature) {
    data.frame(
      name = unname(name),
      title = unname(title),
      exported = unname(exported),
      signature = unname(signature),
      stringsAsFactors = FALSE
    )
  }
  object_name <- function(x) {
    if (is.symbol(x)) {
      return(as.character(x))
    }
    if (is.call(x)) {
      return(gsub(
        "`", "", paste(deparse(x, width.cutoff = 500L), collapse = " ")
      ))
    }
    NA_character_
  }
  is_function_call <- function(x) {
    is.call(x) && identical(x[[1]], as.name("function"))
  }
  rd_text <- function(x) {
    paste(trimws(unlist(x, use.names = FALSE)), collapse = " ")
  }
  scan_expr <- function(expr) {
    rows <- list()
    if (!is.call(expr)) {
      return(rows)
    }

    call_name <- as.character(expr[[1]])[1]
    if (call_name %in% c("<-", "=", "<<-") && length(expr) >= 3) {
      name <- object_name(expr[[2]])
      fun <- expr[[3]]
      if (!is.na(name) && is_function_call(fun)) {
        rows <- c(rows, list(list(
          name = name,
          signature = signature(fun)
        )))
      }
    }
    if (identical(call_name, "assign") && length(expr) >= 3) {
      name <- tryCatch(
        as.character(eval(expr[[2]])),
        error = function(e) NA_character_
      )
      fun <- expr[[3]]
      if (!is.na(name) && is_function_call(fun)) {
        rows <- c(rows, list(list(
          name = name,
          signature = signature(fun)
        )))
      }
    }

    args <- as.list(expr)
    arg_names <- names(args)
    for (i in seq_along(args)[-1]) {
      rows <- c(rows, tryCatch({
        arg <- args[[i]]
        arg_name <- arg_names[i]
        arg_rows <- list()
        if (!is.null(arg_name) && nzchar(arg_name) &&
            is_function_call(arg)) {
          arg_rows <- list(list(
            name = arg_name,
            signature = signature(arg)
          ))
        }
        c(arg_rows, scan_expr(arg))
      }, error = function(e) list()))
    }
    rows
  }

  pkg_is_path <- dir.exists(pkg)
  path <- if (pkg_is_path) {
    normalizePath(pkg, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }

  package <- pkg
  if (pkg_is_path) {
    desc <- file.path(path, "DESCRIPTION")
    if (file.exists(desc)) {
      description <- read.dcf(desc)
      if ("Package" %in% colnames(description)) {
        package <- description[1, "Package"]
      }
    } else {
      package <- basename(path)
    }
  }

  if (is.null(path) && include_source) {
    installed_path <- system.file(package = package)
    if (nzchar(installed_path)) {
      path <- normalizePath(installed_path, winslash = "/", mustWork = TRUE)
    }
  }

  namespace_available <- requireNamespace(package, quietly = TRUE)
  if (include_namespace && !namespace_available && is.null(path)) {
    cli::cli_abort(
      paste(
        "Package {.pkg {package}} is not installed and no source",
        "{.arg pkg} path was supplied."
      ),
      call = NULL
    )
  }

  namespace_exports <- if (namespace_available) {
    getNamespaceExports(package)
  } else {
    character()
  }
  source_exports <- namespace_exports
  if (!is.null(path)) {
    namespace_file <- file.path(path, "NAMESPACE")
  }
  if (!is.null(path) && file.exists(namespace_file)) {
    source_exports <- character()
    expressions <- tryCatch(
      parse(namespace_file),
      error = function(e) expression()
    )
    for (expr in expressions) {
      if (!is.call(expr) || !identical(as.character(expr[[1]]), "export")) {
        next
      }
      source_exports <- c(source_exports, vapply(
        as.list(expr)[-1],
        function(x) gsub(
          "`", "", paste(deparse(x, width.cutoff = 500L), collapse = " ")
        ),
        character(1)
      ))
    }
    source_exports <- unique(source_exports)
  }

  docs <- tryCatch(
    if (pkg_is_path) {
      tools::Rd_db(dir = path)
    } else {
      tools::Rd_db(package = package)
    },
    error = function(e) list()
  )
  titles <- character()
  for (doc in docs) {
    title <- NA_character_
    aliases <- character()
    for (part in doc) {
      tag <- attr(part, "Rd_tag")
      if (identical(tag, "\\title")) {
        title <- gsub("\\s+", " ", rd_text(part))
      }
      if (identical(tag, "\\alias")) {
        aliases <- c(aliases, rd_text(part))
      }
    }
    if (length(aliases) > 0 && !is.na(title)) {
      titles[aliases] <- title
    }
  }

  rows <- list()
  if (include_namespace && namespace_available) {
    namespace <- asNamespace(package)
    object_names <- ls(namespace, all.names = TRUE)
    objects <- lapply(object_names, function(name) {
      tryCatch(
        get(name, envir = namespace, inherits = FALSE),
        error = function(e) NULL
      )
    })
    is_function <- vapply(objects, is.function, logical(1))
    rows <- c(rows, list(row(
      name = object_names[is_function],
      title = titles[object_names[is_function]],
      exported = object_names[is_function] %in% namespace_exports,
      signature = vapply(objects[is_function], signature, character(1))
    )))
  }
  if (include_source && !is.null(path)) {
    r_path <- file.path(path, "R")
    files <- if (dir.exists(r_path)) {
      list.files(r_path, "\\.[rR]$", full.names = TRUE, recursive = TRUE)
    } else {
      character()
    }
    source_rows <- unlist(lapply(files, function(file) {
      expressions <- tryCatch(
        parse(file = file, keep.source = TRUE),
        error = function(e) expression()
      )
      unlist(lapply(as.list(expressions), scan_expr), recursive = FALSE)
    }), recursive = FALSE)
    if (length(source_rows) > 0) {
      rows <- c(rows, list(do.call(rbind, lapply(source_rows, function(x) {
        row(
          name = x$name,
          title = titles[x$name],
          exported = x$name %in% source_exports,
          signature = x$signature
        )
      }))))
    }
  }

  rows <- rows[vapply(rows, nrow, integer(1)) > 0]
  if (length(rows) == 0) {
    return(tibble::as_tibble(empty))
  }

  out <- do.call(rbind, rows)
  out <- do.call(rbind, lapply(unique(out$name), function(name) {
    current <- out[out$name == name, , drop = FALSE]
    signatures <- current$signature[
      !is.na(current$signature) & nzchar(current$signature)
    ]
    if (length(signatures) == 0) {
      signatures <- NA_character_
    }
    titles <- current$title[!is.na(current$title) & nzchar(current$title)]
    if (length(titles) == 0) {
      titles <- NA_character_
    }
    row(
      name = name,
      title = titles[1],
      exported = any(current$exported),
      signature = signatures[1]
    )
  }))
  rownames(out) <- NULL
  tibble::as_tibble(out[order(out$name, na.last = TRUE), , drop = FALSE])
}
