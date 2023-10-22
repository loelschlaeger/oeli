#' Dictionary R6 Object
#'
#' @description
#' Provides a simple key-value interface based on R6.
#'
#' @param key
#' TODO
#' @param alias
#' TODO
#' @param values
#' TODO
#' @param value_types
#' TODO
#' @param allow_overwrite
#' TODO
#' @param alias_allowed
#' TODO
#' @param ...
#' TODO
#' @param key
#' TODO
#' @param value
#' TODO
#'
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
    #' initializing a \code{Dictionary} object
    #' @return
    #' a new \code{Dictionary} object

    initialize = function(
      key, alias, values, value_types, allow_overwrite = FALSE,
      alias_allowed = NULL
    ) {

      # TODO

    },

    #' @description
    #' adding an element
    #' @return
    #' invisibly the \code{Dictionary} object

    add = function(...) {

      # TODO
      invisible(self)

    },

    #' @description
    #' getting elements
    #' @return
    #' the selected value

    get = function(key, value) {

      # TODO

    },

    #' @description
    #' removing elements
    #' @return
    #' invisibly the \code{Dictionary} object

    remove = function(key) {

      # TODO
      invisible(self)

    },

    #' @description
    #' getting keys by alias
    #' @return
    #' a \code{character}

    alias_keys = function(alias) {

      # TODO

    },

    #' @description
    #' printing details of the dictionary
    #' @param ...
    #' currently not used
    #' @return
    #' invisibly the \code{Dictionary} object

    print = function(...) {

      # TODO
      invisible(self)

    }

  ),

  active = list(

    #' @field keys TODO

    keys = function(value) {
      if (missing(value)) {
        # TODO
      } else {
        stop("read only")
      }
    },

    #' @field alias TODO

    alias = function(value) {
      if (missing(value)) {
        # TODO
      } else {
        stop("read only")
      }
    }

  ),

  private = list(

  )
)

