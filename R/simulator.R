#' Simulator R6 Object
#'
#' @description
#' Creates a simulation setup, where a function `f` is evaluated `runs` times,
#' optionally at each combination of input values.
#'
#' Provides some convenience (see below for more details):
#'
#' - Simulation results can be restored from a backup if the R session crashes.
#' - Failed simulation cases can be re-run.
#' - More simulation runs can be conducted after the initial simulation.
#' - Parallel computation and progress updates are supported.
#'
#' @details
#' ## Getting started
#' 1. Initialize a new simulation setup via `object <- Simulator$new()`.
#' 2. Define function `f` and (optionally) arguments via
#'    `object$define(f = f, ...)`.
#' 3. Call `object$go(runs)` for `runs` simulation of `f` evaluated at each
#'    parameter combination.
#' 4. Access the results via `object$results`.
#' 5. The field `object$cases` lists all simulation cases, including those
#'    already resolved and those still pending.
#'
#' See the examples section.
#'
#' ## Backup
#' TODO (with example for defining and using backup)
#'
#' ## Re-run
#' TODO
#'
#' ## More runs
#' TODO
#'
#' ## Parallel computation
#' By default, simulations run sequentially. But since they are independent,
#' they can be parallelized to decrease computation time. To enable parallel
#' computation, use the [`{future}` framework](https://future.futureverse.org/).
#' For example, run
#' \preformatted{
#' future::plan(future::multisession, workers = 4)
#' }
#' in advance for computation in 4 parallel R sessions.
#'
#' ## Progress updates
#' Use the [`{progressr}` framework](https://progressr.futureverse.org/) to
#' get progress updates. For example, run the following in advance:
#' \preformatted{
#' progressr::handlers(global = TRUE)
#' progressr::handlers(
#'   progressr::handler_progress(format = ">> :percent, :eta to go :message")
#' )
#' }
#'
#' @keywords simulation
#' @family simulation helpers
#' @export
#'
#' @examples
#' f <- function(x, y = 1) { Sys.sleep(runif(1)); x + y + rnorm(1, sd = 0.1) }
#' object <- Simulator$new(verbose = TRUE)
#' object$define(f = f, x = as.list(1:2))
#' object$go(runs = 2)
#' object$results
#' object$cases

Simulator <- R6::R6Class(

  classname = "Simulator",
  cloneable = FALSE,

  public = list(

    #' @description
    #' TODO
    #'
    #' @param use_backup \[`NULL` | `character(1)`\]\cr
    #' TODO
    #'
    #' @param verbose \[`logical(1)`\]\cr
    #' TODO

    initialize = function(
      use_backup = NULL, verbose = getOption("verbose", default = FALSE)
    ) {

      private$.verbose <- isTRUE(verbose)

      if (is.null(use_backup)) {

        private$.status(
          "Created new {.cls Simulator}, call {.fun $define} next."
        )

      } else {

        private$.status("Loaded {.cls Simulator} from backup.")

      }

    },

    #' @description
    #' TODO
    #'
    #' @param f \[`function`\]\cr
    #' TODO
    #'
    #' @param ...
    #' TODO

    define = function(f, ...) {

      ### already defined?
      if (!is.null(private$.f)) {
        cli::cli_abort(
          "Definition already provided.",
          call = NULL
        )
      }

      ### input checks
      args <- list(...)
      arg_names <- names(args)
      input_check_response(
        check = list(
          checkmate::check_list(args, len = 0),
          checkmate::check_names(
            arg_names, type = "strict", disjunct.from = private$.reserved_vars
          )
        ),
        var_name = "..."
      )
      for (n_arg in seq_along(args)) {
        input_check_response(
          check = checkmate::check_list(args[[n_arg]]),
          var_name = arg_names[[n_arg]]
        )
      }
      input_check_response(
        check = checkmate::check_function(f, args = arg_names),
        var_name = "f"
      )

      ### save simulation setting
      private$.f <- f
      private$.args <- args

      ### return
      private$.status(
        "Simulation details defined, call {.fun $go} next to run simulations."
      )
      invisible(self)

    },

    #' @description
    #' TODO
    #'
    #' @param runs \[`integer(1)`\]\cr
    #' TODO
    #'
    #' @param backup \[`logical(1)`\]\cr
    #' TODO
    #'
    #' @param path \[`character(1)`\]\cr
    #' TODO

    go = function(
      runs = 0, backup = FALSE,
      path = paste0("backup_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
    ) {

      ### input checks
      input_check_response(
        check = check_missing(runs),
        var_name = "runs"
      )
      input_check_response(
        check = checkmate::check_count(runs, positive = FALSE),
        var_name = "runs"
      )
      input_check_response(
        check = checkmate::check_flag(backup),
        var_name = "backup"
      )
      input_check_response(
        check = checkmate::check_path_for_output(path, overwrite = FALSE),
        var_name = "path"
      )

      ### prepare simulation
      if (backup) dir.create(path)
      private$.new_cases(runs = runs)
      cases <- self$cases |> dplyr::filter(.pending)
      private$.status("Started simulation with {nrow(cases)} cases...")
      progress_step <- progressr::progressor(steps = nrow(cases))

      ### run simulation
      results <- future.apply::future_lapply(cases$.case, function(case) {

        progress_step(glue::glue("[case {case}] started"), amount = 0)

        args <- lapply(cases[case, -(1:4), drop = FALSE], function(x) x[[1]])

        start <- Sys.time()
        f_out <- try_silent(do.call(what = private$.f, args = args))
        end <- Sys.time()
        error <- inherits(f_out, "fail")

        progress_step(glue::glue("[case {case}] finished"), class = "sticky")

        case_out <- if (error) {
          NULL
        } else {
          list(
            ".case" = case,
            ".error" = error,
            ".seconds" = difftime(end, start, units = "secs"),
            ".input" = args,
            ".output" = f_out
          )
        }

        private$.cases[private$.cases$.case == case, ".pending"] <- error
        private$.cases[private$.cases$.case == case, ".error"] <- error

        if (backup && !error) {
          saveRDS(case_out, file = sprintf("%s/case_%03d.rds", path, case))
        }

        return(case_out)

      }, future.seed = TRUE)

      ### return
      private$.results <- c(private$.results, purrr::compact(results))
      private$.status("Simulation completed.")
      invisible(self)

    }

  ),

  active = list(

    #' @field results \[`tibble`, read-only\]\cr
    #' The simulation results.

    results = function(value) {
      if (missing(value)) {
        private$.results |>
          purrr::map(~ tibble::tibble(
            .case = .x$.case,
            .seconds = .x$.seconds,
            .input = list(.x$.input),
            .output = list(.x$.output)
          )) |>
          purrr::list_rbind(ptype = tibble::tibble(
            .case = integer(),
            .seconds = difftime(0, 0),
            .input = list(),
            .output = list()
          )) |>
          dplyr::arrange(.case)
      } else {
        cli::cli_abort(
          "Field {.var $results} is read-only.",
          call = NULL
        )
      }
    },

    #' @field cases \[`tibble`, read-only\]\cr
    #' The simulation cases.

    cases = function(value) {
      if (missing(value)) {
        private$.cases |>
          dplyr::arrange(.case)
      } else {
        cli::cli_abort(
          "Field {.var $cases} is read-only.",
          call = NULL
        )
      }
    }

  ),

  private = list(

    .verbose = FALSE,
    .f = NULL,
    .args = list(),
    .results = list(),
    .cases = tibble::tibble(
      .case = integer(),
      .pending = logical(),
      .error = logical(),
      .run = integer()
    ),
    .reserved_vars = c(".case", ".pending", ".error", ".run"),

    .status = function(msg, verbose = private$.verbose) {
      checkmate::assert_string(msg)
      checkmate::assert_flag(verbose)
      if (verbose) cli::cli_inform(msg, .envir = parent.frame())
    },

    .new_cases = function(runs) {
      run_seq <- length(unique(private$.cases$.run)) + seq_len(runs)
      grid <- c(list(.run = run_seq), private$.args) |>
        expand.grid(stringsAsFactors = FALSE) |>
        tibble::as_tibble() |>
        dplyr::arrange(.run)
      new_cases <- cbind(
        tibble::tibble(
          .case = nrow(self$cases) + seq_len(nrow(grid)),
          .pending = TRUE,
          .error = FALSE
        ),
        grid
      )
      private$.cases <- dplyr::bind_rows(private$.cases, new_cases)
    }
  )

)
