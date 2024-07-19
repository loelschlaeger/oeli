#' Dictionary R6 Object
#'
#' @description
#' Provides a simple key-value interface based on R6.
#'
#' @param key_name \[`character(1)`\]\cr
#' The name for the key variable.
#'
#' @param alias_name \[`NULL` | `character(1)`\]\cr
#' Optionally the name for the alias variable.
#'
#' @param value_names \[`character(0)`\]\cr
#' The names of the values connected to a key.
#'
#' @param value_assert \[`alist(1)`\]\cr
#' For each element in \code{value_names}, \code{values_assert} *can* have an
#' identically named element of the form \code{checkmate::assert_*(...)}, where
#' \code{...} can be any argument for the assertion function except for the
#' \code{x} argument.
#'
#' @param allow_overwrite \[`logical(1)`\]\cr
#' Allow overwriting existing keys with new values?
#' Duplicate keys are never allowed.
#'
#' @param keys_reserved \[`character()`\]\cr
#' Names that must not be used as keys.
#'
#' @param alias_choices \[`NULL` or `character()`\]\cr
#' Optionally possible values for the alias. Can also be \code{NULL}, then all
#' alias values are allowed.
#'
#' @param dictionary_name \[`NULL` or `character()`\]\cr
#' Optionally the name for the dictionary.
#'
#' @param key \[`character(1)`\]\cr
#' A value for the key variable \code{key_name}. Use the \code{$keys} method for
#' available keys.
#'
#' @keywords indexing
#' @family package helpers
#' @export
#'
#' @examples
#' # TODO

Dictionary <- R6::R6Class(
  classname = "Dictionary",
  lock_class = TRUE,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Initializing a new \code{Dictionary} object.
    initialize = function(
      key_name,
      alias_name = NULL,
      value_names = character(),
      value_assert = alist(),
      allow_overwrite = TRUE,
      keys_reserved = character(),
      alias_choices = NULL,
      dictionary_name = NULL
    ) {
      checkmate::assert_string(key_name)
      checkmate::assert_string(alias_name, null.ok = TRUE)
      checkmate::assert_character(
        value_names,
        any.missing = FALSE, unique = TRUE
      )
      checkmate::assert_list(
        value_assert,
        types = "call", any.missing = FALSE
      )
      checkmate::assert_names(
        names(value_assert),
        type = "unique", subset.of = value_names
      )
      checkmate::assert_flag(allow_overwrite)
      checkmate::assert_character(
        keys_reserved,
        any.missing = FALSE, unique = TRUE
      )
      checkmate::assert_character(
        alias_choices,
        any.missing = FALSE, unique = TRUE, null.ok = TRUE
      )
      checkmate::assert_string(dictionary_name, null.ok = TRUE)
      private$.key_name <- key_name
      if (is.null(alias_name)) {
        private$.alias_activated <- FALSE
      } else {
        private$.alias_activated <- TRUE
        private$.alias_name <- alias_name
      }
      private$.value_names <- value_names
      private$.value_assert <- value_assert
      private$.allow_overwrite <- allow_overwrite
      private$.keys_reserved <- keys_reserved
      private$.alias_choices <- alias_choices
      private$.dictionary_name <- dictionary_name
    },

    #' @description
    #' Adding an element to the dictionary.
    #' @param ...
    #' Values for
    #' - the key variable \code{key_name} (must be a single \code{character}),
    #' - the alias variable \code{alias_name} (optionally, must then be a
    #'   \code{character} \code{vector}),
    #' - all the variables specified for \code{value_names} (if any, they must
    #'   comply to the \code{value_assert} checks).

    add = function(...) {
      inputs <- list(...)
      private$.check_inputs(inputs)
      private$.add_to_storage(inputs)
      invisible(self)
    },

    #' @description
    #' Getting elements from the dictionary.
    #' @param value \[`NULL` | `character(1)`\]\cr
    #' One of the elements in \code{value_names}, selecting the required value.
    #' Can also be \code{NULL} (default) for all values connected to the
    #' \code{key}, returned as a \code{list}.

    get = function(key, value = NULL) {
      if (!private$.key_exists(key)) {
        stop("The key '", key, "' does not exist.", call. = FALSE)
      }
      if (is.null(value)) {
        private$.storage[[key]]
      } else {
        if (value %in% private$.value_names) {
          private$.storage[[key]][[value]]
        } else {
          stop("The value name '", value, "' does not exist.", call. = FALSE)
        }
      }
    },

    #' @description
    #' Removing elements from the dictionary (and associated alias, if any).

    remove = function(key) {
      if (!private$.key_exists(key)) {
        warning("The key '", key, "' does not exist.", call. = FALSE)
      } else {
        private$.storage <- within(private$.storage, rm(list = key))
        if (private$.alias_activated) {
          private$.alias_list <- lapply(
            private$.alias_list, function(alias) alias[!key %in% alias]
          )
        }
      }
      invisible(self)
    },

    #' @description
    #' Printing details of the dictionary.

    print = function() {
      cat("<Dictionary>", private$.dictionary_name, "\n")
      if (length(self$keys) == 0) {
        cat("No elements contained.")
      } else {
        cat("Keys: \n")
        cat(paste("-", self$keys), sep = "\n")
      }
      invisible(self)
    }
  ),
  active = list(

    #' @field keys \[`character()`\]\cr
    #' Available keys.
    keys = function(value) {
      if (missing(value)) {
        names(private$.storage)
      } else {
        stop("read only")
      }
    },

    #' @field alias \[`list()`\]\cr
    #' Available keys per alias value.

    alias = function(value) {
      if (missing(value)) {
        private$.alias_list
      } else {
        stop("read only")
      }
    }
  ),

  private = list(
    .storage = list(),
    .alias_list = list(),
    .key_name = NA_character_,
    .alias_name = NULL,
    .alias_activated = NA,
    .value_names = character(),
    .value_assert = alist(),
    .allow_overwrite = NA,
    .keys_reserved = character(),
    .alias_choices = NULL,
    .dictionary_name = NULL,

    .check_inputs = function(inputs) {
      input_names_required <- c(private$.key_name, private$.value_names)
      checkmate::assert_names(
        names(inputs),
        subset.of = c(input_names_required, private$.alias_name)
      )
      checkmate::assert_string(inputs[[private$.key_name]])
      if (private$.alias_activated && !is.null(private$.alias_choices)) {
        checkmate::assert_subset(
          inputs[[private$.alias_name]],
          choices = private$.alias_choices
        )
      }
      if (inputs[[private$.key_name]] %in% private$.keys_reserved) {
        stop(
          "The key '", inputs[[private$.key_name]], "' must not be used.",
          call. = FALSE
        )
      }
      if (private$.key_exists(inputs[[private$.key_name]]) &&
        !private$.allow_overwrite
      ) {
        stop(
          "The key '", inputs[[private$.key_name]], "' already exists.",
          call. = FALSE
        )
      }
      for (value_name in private$.value_names) {
        if (value_name %in% names(private$.value_assert)) {
          assert_function <- private$.value_assert[[value_name]]
          assert_function[["x"]] <- inputs[[value_name]]
          eval(assert_function)
        }
      }
    },

    .add_to_storage = function(inputs) {
      key <- inputs[[private$.key_name]]
      private$.storage[[key]] <- inputs[private$.value_names]
      if (private$.alias_activated && !is.null(inputs[[private$.alias_name]])) {
        for (alias in inputs[[private$.alias_name]]) {
          private$.alias_list[[alias]] <- c(private$.alias_list[[alias]], key)
        }
      }
    },

    .key_exists = function(key) {
      key %in% self$keys
    }
  )
)
