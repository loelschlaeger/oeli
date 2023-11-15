#' Index R6 Object
#'
#' @description
#' Provides a simple indexing interface for list elements based on R6.
#' Basically, it allows to store items in a list and to regain them based on
#' identifiers defined by the user.
#'
#' @section Setting identifiers:
#' An identifier is a \code{character}, typically a binary property. Identifiers
#' can be negated by placing an exclamation mark (\code{"!"}) in front of them.
#' Identifiers that have been assigned to other elements previously do not need
#' to be specified again for new elements; instead, a default value can be used.
#' This default value can be defined either globally for all cases (via the
#' \code{$missing_identifier} field) or separately for each specific case (via
#' the method argument).
#'
#' @section User confirmation:
#' If desired, the user can be asked for confirmation when adding, extracting,
#' or removing elements using identifiers. This behavior can be set globally
#' through the \code{$confirm} field or customized separately for each specific
#' case via the method argument.
#'
#' @param identifier
#' a \code{character} \code{vector} with one or more identifiers (the identifier
#' \code{"all"} is reserved to select all elements)
#' @param ids
#' an \code{integer} \code{vector} of one or more ids
#' @param confirm
#' either \code{TRUE} to be prompted for confirmation, or \code{FALSE} else
#' @param missing_identifier
#' the \code{logical} value for not specified identifiers (either \code{NA},
#' \code{TRUE}, or \code{FALSE})
#'
#' @return
#' The output depends on the method:
#' - \code{$new()} returns an \code{Index} object.
#' - \code{$add()}, \code{$remove()}, and \code{$print()} invisibly return the
#'   \code{Index} object (to allow for method chaining)
#' - \code{$get()} returns the requested element(s)
#' - \code{$number()} returns an \code{integer}
#' - \code{$indices()} return an \code{integer} \code{vector}
#'
#' @export
#'
#' @examples
#' ### 1. Create an `Index` object:
#' my_index <- Index$new()
#'
#' # 2. Add elements along with identifiers:
#' my_index$
#'   add(42, c("number", "rational"))$
#'   add(pi, c("number", "!rational"))$
#'   add("fear of black cats", c("text", "!rational"))$
#'   add("wearing a seat belt", c("text", "rational"))$
#'   add(mean, "function")
#'
#' # 3. What elements are stored?
#' print(my_index)
#'
#' # 4. Extract elements based on identifiers:
#' my_index$get("rational")
#' my_index$get("!rational")
#' my_index$get(c("text", "!rational"))
#' my_index$get("all")                        # get all elements
#'
#' # 5. Extract elements based on ids:
#' my_index$get(ids = 4:5)
#' my_index$get(ids = 4:5, id_names = TRUE)   # add the ids as names

Index <- R6::R6Class(

  classname = "Index",
  lock_class = TRUE,
  cloneable = FALSE,

  public = list(

    #' @description
    #' initializing an \code{Index} object
    #' @return
    #' a new \code{Index} object

    initialize = function() {

      ### elements are saved in a list
      private$elements <- list()

      ### ids are saved in a data.frame with identifiers in columns
      private$ids <- data.frame()

    },

    #' @description
    #' adding an element
    #' @param x
    #' any object to be saved
    #' @return
    #' invisibly the \code{Index} object

    add = function(
      x, identifier, confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier
    ) {

      ### input checks
      if (missing(x)) {
        stop("please specify the object 'x' to be added", call = FALSE)
      }
      if (missing(identifier)) {
        stop("please specify at least one entry for 'identifier'", call = FALSE)
      }
      private$check_input(
        identifier = identifier, confirm = confirm,
        missing_identifier = missing_identifier
      )
      if (any(c("all", "!all") %in% identifier)) {
        stop("the identifier \"all\" is reserved", call. = FALSE)
      }

      ### inform user about action and request confirmation
      if (confirm) {
        private$user_confirm(
          action = paste0("add a <", class(x), ">"),
          identifier = identifier, missing_identifier = missing_identifier,
          complete = TRUE
        )
      }

      ### add element
      private$add_element(x, identifier, missing_identifier)
      invisible(self)

    },

    #' @description
    #' getting elements
    #' @param id_names
    #' either \code{TRUE} to name the elements according to their ids or
    #' \code{FALSE} if not
    #' @return
    #' the selected object(s)

    get = function(
      identifier = character(), ids = integer(),
      confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier,
      id_names = FALSE
    ) {

      ### input checks
      private$check_input(
        identifier = identifier, confirm = confirm, ids = ids,
        missing_identifier = missing_identifier
      )

      ### check for unknown identifier
      if (length(identifier) > 0) {
        identifier <- private$check_identifier_known(identifier)
      } else if (length(ids) == 0) {
        stop("please specify either 'identifier' or 'ids'")
      }

      ### inform user about action and request confirmation
      if (confirm) {
        private$user_confirm(
          action = "extract", identifier = identifier, ids = ids,
          missing_identifier = missing_identifier, complete = FALSE
        )
      }

      ### get elements
      private$get_element(
        identifier = identifier, ids = ids, id_names = id_names
      )

    },

    #' @description
    #' removing elements
    #' @param shift_ids
    #' either \code{TRUE} to shift ids when in-between elements are removed,
    #' or \code{TRUE} to keep the ids
    #' @return
    #' invisibly the \code{Index} object

    remove = function(
      identifier = character(), ids = integer(),
      confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier, shift_ids = TRUE
    ) {

      ### input checks
      private$check_input(
        identifier = identifier, confirm = confirm, ids = ids,
        missing_identifier = missing_identifier
      )
      checkmate::assert_flag(shift_ids)

      ### check for unknown identifier
      if (length(identifier) > 0) {
        identifier <- private$check_identifier_known(identifier)
      } else if (length(ids) == 0) {
        stop("please specify either 'identifier' or 'ids'")
      }

      ### inform user about action and request confirmation
      if (confirm) {
        private$user_confirm(
          action = "remove", identifier = identifier, ids = ids,
          missing_identifier = missing_identifier, complete = FALSE
        )
      }

      ### remove elements
      ids <- private$merge_ids(ids, private$get_ids(identifier = identifier))
      if (shift_ids) {
        private$elements[ids] <- NULL
        private$ids <- private$ids[-ids, ]
      } else {
        private$elements[ids] <- list(NULL)
        private$ids[ids, ] <- NA
      }
      invisible(self)

    },

    #' @description
    #' computing the number of identified elements
    #' @return
    #' an \code{integer}

    number = function(
      identifier = "all", missing_identifier = self$missing_identifier,
      confirm = FALSE
    ) {

      ### input checks
      private$check_input(
        identifier = identifier, missing_identifier = missing_identifier,
        confirm = confirm
      )

      ### check for unknown identifier
      if (length(identifier) > 0) {
        identifier <- private$check_identifier_known(identifier)
      }

      ### inform user about action and request confirmation
      if (confirm) {
        private$user_confirm(
          action = "count", identifier = identifier,
          missing_identifier = missing_identifier, complete = FALSE
        )
      }

      ### perform action
      length(self$indices(identifier, confirm = confirm))

    },

    #' @description
    #' returning indices based on defined identifiers
    #' @return
    #' an \code{integer} \code{vector}

    indices = function(
      identifier = "all", confirm = interactive() & self$confirm
    ) {

      private$check_input(identifier, confirm)

      ### check for unknown identifier
      if (length(identifier) > 0) {
        identifier <- private$check_identifier_known(identifier)
      }

      ### inform user about action and request confirmation
      if (confirm) {
        private$user_confirm(
          action = "get indices of",
          identifier = identifier, missing_identifier = missing_identifier,
          complete = FALSE
        )
      }

      ### perform action
      private$get_ids(identifier = identifier)

    },

    #' @description
    #' printing details of the saved elements
    #' @param ...
    #' currently not used
    #' @return
    #' invisibly the \code{Index} object

    print = function(...) {
      nelements <- length(private$elements)
      if (nelements > 0) {
        cat("number of elements:", self$number(), "\n")
        cat("identifiers used:", self$identifier, "\n")
      } else {
        cat("no elements saved yet")
      }
      invisible(self)
    }

  ),

  active = list(

    #' @field identifier a \code{character} \code{vector}, the identifiers used

    identifier = function(value) {
      if (missing(value)) {
        colnames(private$ids)
      } else {
        stop("read only")
      }
    },

    #' @field confirm setting the default value for confirmations (either
    #' \code{TRUE} or \code{FALSE})

    confirm = function(value) {
      if (missing(value)) {
        private$confirm_default
      } else {
        checkmate::assert_flag(value)
        private$confirm_default <- value
      }
    },

    #' @field missing_identifier setting the default value for not specified
    #' identifiers (either \code{TRUE}, \code{FALSE}, or \code{NA})

    missing_identifier = function(value) {
      if (missing(value)) {
        private$missing_default
      } else {
        checkmate::assert_flag(value, na.ok = TRUE)
        private$missing_default <- value
      }
    },

    #' @field hide_warnings either \code{TRUE} to hide warnings (for example
    #' if unknown identifiers are selected) or \code{FALSE} (default), else

    hide_warnings = function(value) {
      if (missing(value)) {
        private$.hide_warnings
      } else {
        checkmate::assert_flag(value, na.ok = TRUE)
        private$.hide_warnings <- value
      }
    }

  ),

  private = list(

    elements = NULL,
    ids = NULL,

    confirm_default = FALSE,
    missing_default = NA,
    .hide_warnings = FALSE,

    check_input = function(
      identifier = NULL, confirm = NULL, ids = NULL, missing_identifier = NULL
    ) {

      ### check 'identifier' input
      if (!is.null(identifier)) {
        checkmate::assert_character(
          identifier, any.missing = FALSE, unique = TRUE
        )
      }

      ### check 'confirm' input
      checkmate::assert_flag(confirm)

      ### check 'ids' input
      if (!is.null(ids)) {
        checkmate::assert_integerish(
          ids, lower = 1, any.missing = FALSE, unique = TRUE
        )
      } else if (length(identifier) == 0) {
        stop("need at least one identifier", call. = FALSE)
      }

      ### check 'missing_identifier' input
      if (!is.null(missing_identifier)) {
        checkmate::assert_flag(missing_identifier, na.ok = TRUE)
      }

    },

    check_identifier_known = function(identifier) {
      checkmate::assert_character(
        identifier, any.missing = FALSE, min.len = 1, unique = TRUE
      )
      if ("all" %in% identifier) {
        return("all")
      }
      identifier_translated <- names(private$translate_identifier(identifier))
      unknown <- which(!identifier_translated %in% self$identifier)
      if (length(unknown) > 0) {
        if (!self$hide_warnings) {
          warning(
            paste0(
              "I do not know the identifier(s) '",
              paste(identifier[unknown], collapse = "', '"),
              "' and hence I will ignore them."
            ),
            call. = FALSE, immediate. = TRUE
          )
        }
        identifier <- identifier[-unknown]
      }
      return(identifier)
    },

    user_confirm = function(
      action = character(), identifier = character(), ids = integer(),
      missing_identifier = self$missing_identifier, complete = TRUE
    ) {
      checkmate::assert_flag(complete)
      cat("You are about to", action)
      if (any(identifier == "all")) {
        cat(" all elements.\n")
      } else {
        cat(" element(s)")
        if (length(ids) > 0) {
          cat(" with ids\n")
          print(ids)
          if (length(identifier) > 0) {
            cat("and ")
          }
        } else {
          cat(" ")
        }
        if (length(identifier) > 0) {
          cat("with identifiers:\n")
          identifier_bool <- private$translate_identifier(identifier)
          if (complete) {
            identifier_bool <- private$complete_identifier_bool(
              identifier_bool = identifier_bool,
              missing_identifier = missing_identifier
            )
          }
          print(identifier_bool)
        }
      }
      confirmation <- user_confirm("Is that alright?", default = TRUE)
      if (!confirmation) {
        stop("Okay, then no.", call. = FALSE)
        return(invisible(self))
      }
    },

    add_identifier = function(identifier, missing_identifier) {
      checkmate::assert_character(
        identifier, any.missing = FALSE, min.len = 1, unique = TRUE
      )
      stopifnot(length(intersect(identifier, self$identifier)) == 0)
      if (nrow(private$ids) == 0) {
        private$ids[1, ] <- NA
        private$ids[, identifier] <- NA
        private$ids <- private$ids[0, ]
      } else {
        private$ids[, identifier] <- missing_identifier
      }
    },

    add_element = function(x, identifier, missing_identifier) {
      identifier_bool <- private$translate_identifier(identifier)
      new_identifier <- setdiff(names(identifier_bool), self$identifier)
      if (length(new_identifier) > 0) {
        private$add_identifier(new_identifier, missing_identifier)
      }
      id <- private$next_id()
      private$elements[[id]] <- x
      private$ids[id, ] <- self$missing_identifier
      private$ids[id, names(identifier_bool)] <- identifier_bool
    },

    next_id = function() {
      length(private$elements) + 1
    },

    get_element = function(identifier, ids, id_names) {
      checkmate::assert_flag(id_names)
      ids <- private$merge_ids(
        ids,
        private$get_ids(identifier = identifier)
      )
      structure(
        private$elements[ids],
        names = if (id_names) ids
      )
    },

    translate_identifier = function(identifier) {
      identifier_translated <- logical(0)
      for (value in identifier) {
        if (startsWith(value, "!")) {
          identifier_translated[substring(value, first = 2)] <- FALSE
        } else {
          identifier_translated[value] <- TRUE
        }
      }
      return(identifier_translated)
    },

    complete_identifier_bool = function(identifier_bool, missing_identifier) {
      identifier_bool[setdiff(self$identifier, names(identifier_bool))] <-
        missing_identifier
      return(identifier_bool)
    },

    get_ids = function(identifier) {
      if (length(identifier) == 0) {
        return(integer())
      }
      if (any(identifier == "all")) {
        return(seq_along(private$elements))
      }
      identifier_bool <- private$translate_identifier(identifier)
      ids <- apply(
        private$ids[names(identifier_bool)],
        1,
        identical,
        identifier_bool
      )
      sort(unique(which(ids)))
    },

    merge_ids = function(...) {
      ids <- unlist(list(...))
      checkmate::assert_integerish(ids, lower = 1, any.missing = FALSE)
      if (length(ids) == 0) {
        return(integer())
      } else {
        sort(unique(ids))
      }
    }

  )
)

#' Merge named lists
#'
#' @description
#' This function merges \code{list}s based on their element names. Elements are
#' only included in the final output \code{list}, if no former \code{list} has
#' contributed an element with the same name.
#'
#' @param ...
#' One or more named \code{list}(s).
#'
#' @return
#' A \code{list}.
#'
#' @examples
#' merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4))
#'
#' @export

merge_lists <- function(...) {
  inputs <- list(...)
  lapply(inputs, function(input) checkmate::assert_list(input, names = "unique"))
  final <- list()
  for (input in inputs) {
    for (element in names(input)) {
      if (!element %in% names(final)) {
        final[[element]] <- input[[element]]
      }
    }
  }
  return(final)
}



