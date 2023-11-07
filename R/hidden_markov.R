#' Compute HMM log-likelihood
#'
#' @description
#' This function computes the log-likelihood of a hidden Markov model.
#'
#' @param theta
#' A `numeric` vector, the model parameters (if any) in the following order:
#' 1. column-wise non-diagonal entries of the transition probability matrix
#' 2. the initial state distribution
#' 3. parameters of the state-dependent distribution for each state
#' @param data
#' A `numeric` vector, the time series data.
#' @inheritParams hmm
#' @inheritParams parameter
#' @param neg
#' Set to `TRUE` to return the negative log-likelihood value.
#'
#' @examples
#' states <- 2
#' sdd <- "normal"
#' x <- simulate_hmm(
#'   Tp = 100, states = states, sdd = sdd, mean = c(-1, 1), sd = c(1, 1)
#' )
#' theta <- as.vector(x$parameter)
#' ll_hmm(theta = theta, data = x$data, states = states, sdd = sdd)
#'
#' @return
#' A `numeric`, the log-likelihood value at `theta` given `data`.
#'
#' @export

ll_hmm <- function(theta, data, states, sdd, neg = FALSE, ...) {
  checkmate::assert_numeric(data)
  checkmate::assert_flag(neg)
  hmm <- hmm(states = states, sdd = sdd)
  par <- as.list.parameter(theta, hmm = hmm, ...)
  Tp <- length(data)
  allprobs <- matrix(1, nrow = length(data), ncol = hmm$states)
  for (s in seq_len(ncol(allprobs))){
    allprobs[, s] <- sdd_call(
      sdd = hmm$sdd, par_sdd = par$parameter_sdd[[s]], mode = "density",
      x = data
    )
  }
  foo <- par$init %*% diag(allprobs[1, ])
  llk <- log(sum(foo))
  phi <- foo / sum(foo)
  for(t in seq_len(Tp)[-1]){
    foo <- phi %*% par$tpm %*% diag(allprobs[t, ])
    llk <- llk + log(sum(foo))
    phi <- foo / sum(foo)
  }
  ifelse(neg, -llk, llk)
}

#' Simulate HMM data
#'
#' @description
#' This function simulates a times series of a hidden Markov model.
#'
#' @param Tp
#' An `integer`, the number of observations.
#' @inheritParams hmm
#' @inheritParams parameter
#'
#' @return
#' A `list` with three elements:
#' - `"data"` (the simulated time series)
#' - `"states"` (the simulated states)
#' - `"parameter"` (the model parameters)
#'
#' @examples
#' x <- simulate_hmm(
#'   Tp = 100, states = 2, sdd = "normal", mean = c(-1, 1), sd = c(1, 1)
#' )
#' str(x)
#' plot(x$data, col = x$states, type = "h")
#'
#' @export

simulate_hmm <- function(Tp, states, sdd, ...) {
  checkmate::assert_int(Tp, lower = 1)
  hmm <- hmm(states = states, sdd = sdd)
  states <- hmm$states
  sdd <- hmm$sdd
  parameter <- parameter(hmm = hmm, ..., validate = TRUE)
  tpm <- parameter$tpm
  init <- parameter$init
  par_sdd <- parameter$parameter_sdd
  s <- numeric(Tp)
  s[1] <- sample.int(states, size = 1, prob = init)
  x <- numeric(Tp)
  x[1] <- sdd_call(sdd = sdd, par_sdd = par_sdd[[s[1]]], mode = "rng", n = 1)
  for (t in seq_len(Tp)[-1]) {
    s[t] <- sample.int(states, size = 1, prob = tpm[s[t - 1], ])
    x[t] <- sdd_call(
      sdd = sdd, par_sdd = par_sdd[[s[t]]], mode = "rng", n = 1
    )
  }
  list("data" = x, "states" = s, "parameter" = parameter)
}

#' Define HMM model
#'
#' @description
#' This function defines a hidden Markov model.
#'
#' @param states
#' An `integer`, the number of states.
#' @param sdd
#' A `character`, defining the type of state-dependent distribution.
#' Print `sdds_dictionary` for possible values.
#'
#' @return
#' An object of class `hmm`.

hmm <- function(states, sdd) {
  checkmate::assert_int(states, lower = 2)
  checkmate::assert_choice(sdd, sdds_dictionary$keys)
  structure(
    list(
      "states" = as.integer(states),
      "sdd" = sdds_dictionary$get(sdd),
      "sdd_name" = sdd
    ),
    class = c("hmm", "list")
  )
}

#' @rdname hmm
#' @param x
#' An object of class `hmm`.
#' @param ...
#' Currently not used.
#' @exportS3Method

print.hmm <- function(x, ...) {
  checkmate::assert_class(x, "hmm")
  cat("states:", x$states, "\n")
  cat("distribution:", x$sdd_name)
}

#' Dictionary of state-dependent distributions
#'
#' @description
#' The `sdds_dictionary` object is a dictionary of currently implemented
#' state-dependent distributions.
#'
#' @format
#' The `sdds_dictionary` is an \code{\link{Dictionary}} object.
#'
#' @export

sdds_dictionary <- Dictionary$new(
  "key_name" = "name",
  "value_names" = c(
    "density",
    "distribution",
    "quantile",
    "rng",
    "parameter_names",
    "parameter_lower_bound",
    "parameter_upper_bound"
  ),
  value_assert = alist(
    "density" = checkmate::assert_function(),
    "distribution" = checkmate::assert_function(),
    "quantile" = checkmate::assert_function(),
    "rng" = checkmate::assert_function(),
    "parameter_names" = checkmate::assert_character(any.missing = FALSE, unique = TRUE),
    "parameter_lower_bound" = checkmate::assert_numeric(any.missing = FALSE),
    "parameter_upper_bound"= checkmate::assert_numeric(any.missing = FALSE)
  ),
  allow_overwrite = FALSE,
  dictionary_name = "Available state-dependent distributions"
)$add(
  "name" = "normal",
  "density" = stats::dnorm,
  "distribution" = stats::pnorm,
  "quantile" = stats::qnorm,
  "rng" = stats::rnorm,
  "parameter_names" = c("mean", "sd"),
  "parameter_lower_bound" = c(-Inf, 0),
  "parameter_upper_bound" = c(Inf, Inf)
)$add(
  "name" = "poisson",
  "density" = stats::dpois,
  "distribution" = stats::ppois,
  "quantile" = stats::qpois,
  "rng" = stats::rpois,
  "parameter_names" = "lambda",
  "parameter_lower_bound" = 0,
  "parameter_upper_bound" = Inf
)

#' Call a state-dependent distribution
#'
#' @description
#' This helper function evaluates the density, distribution, quantile, or random
#' number generator of a state-dependent distribution.
#'
#' @param sdd
#' An element of `sdds_dictionary`.
#' @param par_sdd
#' A named `list` of parameters for the state-dependent distribution.
#' @param mode
#' Either `"density"`, `"distribution"`, `"quantile"`, or `"rng"`.
#' @param ...
#' Additional arguments.
#'
#' @return
#' A \code{numeric}.
#'
#' @keywords internal

sdd_call <- function(sdd, par_sdd, mode, ...) {
  checkmate::assert_choice(
    mode, choice = c("density", "distribution", "quantile", "rng")
  )
  what <- sdd[[mode]]
  checkmate::assert_function(x = what)
  checkmate::assert_list(par_sdd, any.missing = FALSE, names = "strict")
  args <- c(par_sdd, list(...))
  checkmate::assert_subset(
    function_arguments(
      f = what, with_default = FALSE, with_ellipsis = FALSE
    ),
    choices = names(args)
  )
  do.call(what = what, args = args)
}

#' Define HMM parameter
#'
#' @description
#' This function defines parameters of a hidden Markov model.
#'
#' @param hmm
#' An object of class \code{\link{hmm}}.
#' @param x
#' An object of class \code{\link{parameter}}.
#' @param ...
#' Different HMM parameters:
#' - \code{"tpm"}, the transition probability matrix (optional)
#' - \code{"init"}, the initial state distribution or \code{"stationary"}
#'   for the stationary distribution of \code{"tpm"} (optional)
#' - parameters of the state-dependent distribution, see \code{hmm$sdd}
#'   (required)
#' @param validate
#' Either \code{TRUE} to validate the parameters (i.e., check for completeness
#' and restrictions) or \code{FALSE}, else.
#'
#' @return
#' An object of class \code{\link{parameter}}.

parameter <- function(hmm, ..., validate = FALSE) {
  checkmate::assert_class(hmm, "hmm")
  checkmate::assert_flag(validate)
  states <- hmm$states
  template <- parameter_template(hmm)
  pars <- list(...)
  par_names <- if (length(pars) == 0) character() else names(pars)
  if (!"tpm" %in% par_names) {
    tpm <- sample_transition_probability_matrix(dim = states)
  } else {
    tpm <- pars$tpm
  }
  template$tpm <- tpm
  if (!"init" %in% par_names || identical(par_names[["init"]], "stationary")) {
    init <- stationary_distribution(tpm = tpm, soft_fail = TRUE)
  } else {
    init <- pars$init
  }
  template$init <- init
  for (par in hmm$sdd$parameter_names) {
    if (par %in% par_names) {
      for (state in seq_len(states)) {
        template$parameter_sdd[[state]][[par]] <- pars[[par]][state]
      }
    }
  }
  if (validate) {
    validate.parameter(template, hmm)
  } else {
    return(template)
  }
}

#' @rdname parameter

parameter_template <- function(hmm) {
  checkmate::assert_class(hmm, "hmm")
  states <- hmm$states
  template <- list(
    "tpm" = NA_real_,
    "init" = NA_real_,
    "parameter_sdd" = vector(mode = "list", length = states)
  )
  names(template) <- c("tpm", "init", "parameter_sdd")
  sdd_par <- hmm$sdd$parameter_names
  for (state in seq_len(states)) {
    template$parameter_sdd[[state]] <- stats::setNames(
      as.list(rep(NA_real_, length(sdd_par))),
      sdd_par
    )
  }
  structure(template, class = c("parameter", "list"))
}

#' @rdname parameter
#' @exportS3Method

print.parameter <- function(x, ...) {
  checkmate::assert_class(x, "parameter")
  if (checkmate::test_list(x)) {
    print_matrix(x$tpm, label = "- tpm", simplify = TRUE)
    cat("\n")
    print_matrix(x$init, label = "- init", simplify = TRUE)
    cat("\n")
    for (state in seq_along(x$parameter_sdd)) {
      cat("- state", state, ":\n")
      print(unlist(x$parameter_sdd[[state]]))
    }
  } else {
    print(unclass(x))
  }
  invisible(x)
}

#' @rdname parameter

validate.parameter <- function(x, hmm) {
  checkmate::assert_class(x, "parameter")
  checkmate::assert_class(hmm, "hmm")
  states <- hmm$states
  sdd <- hmm$sdd
  assert_transition_probability_matrix(x$tpm, dim = states)
  assert_probability_vector(x$init, len = states)
  for (state in seq_len(states)) {
    for (par in seq_along(sdd$parameter_names)) {
      par_name <- sdd$parameter_names[par]
      checkmate::assert_number(
        x$parameter_sdd[[state]][[par_name]],
        lower = sdd$parameter_lower_bound[par],
        upper = sdd$parameter_upper_bound[par],
        .var.name = paste0("x$parameter_sdd[[", state, "]][[", par_name, "]]")
      )
    }
  }
  return(x)
}

#' @rdname parameter
#' @param mode
#' Ignored.
#' @exportS3Method

as.vector.parameter <- function(x, mode = "numeric") {
  checkmate::assert_class(x, "parameter")
  if (checkmate::test_numeric(x)) {
    return(x)
  }
  par <- numeric()
  par_names <- character()
  if ("tpm" %in% names(x)) {
    add_par <- x$tpm[row(x$tpm) != (col(x$tpm))]
    names(add_par) <- matrix_indices(x$tpm, "tpm*_", TRUE)
    par <- c(par, add_par)
  }
  if ("init" %in% names(x)) {
    add_par <- x$init[-1]
    names(add_par) <- paste("init*", seq_along(x$init)[-1], sep = "_")
    par <- c(par, add_par)
  }
  if ("parameter_sdd" %in% names(x)) {
    for (s in seq_along(x$parameter_sdd)) {
      add_par <- unlist(x$parameter_sdd[[s]])
      names(add_par) <- paste(names(add_par), s, sep = "_")
      par <- c(par, add_par)
    }
  }
  structure(par, class = c("parameter", "numeric"))
}

#' @rdname parameter
#' @exportS3Method

as.list.parameter <- function(x, hmm, ..., validate = FALSE) {
  checkmate::assert_flag(validate)
  if (checkmate::test_class(x, c("parameter", "list"))) {
    return(x)
  }
  checkmate::assert_class(hmm, "hmm")
  states <- hmm$states
  template <- parameter_template(hmm)
  pars <- list(...)
  par_names <- if (length(pars) == 0) character() else names(pars)
  if ("tpm" %in% par_names) {
    tpm <- pars$tpm
  } else {
    tpm <- matrix(1, nrow = states, ncol = states)
    tpm[row(tpm) != col(tpm)] <- x[1:(states * (states - 1))]
    x <- x[-(1:(states * (states - 1)))]
    tpm <- tpm / rowSums(tpm)
  }
  template$tpm <- tpm
  if ("init" %in% par_names) {
    if (identical(par_names[["init"]], "stationary")) {
      init <- stationary_distribution(tpm = tpm, soft_fail = TRUE)
    } else {
      init <- pars$init
    }
  } else {
    init <- c(1, x[seq_len(states - 1)])
    init <- init / sum(init)
    x <- x[-seq_len(states - 1)]
  }
  template$init <- init
  for (state in seq_len(states)) {
    for (par in hmm$sdd$parameter_names) {
      if (par %in% par_names) {
        value <- pars[[par]][state]
      } else {
        value <- x[1]
        x <- x[-1]
      }
      template$parameter_sdd[[state]][[par]] <- unname(value)
    }
  }
  if (length(x) > 0) {
    warning("Something is odd, not all parameters from 'x' were used.")
  }
  if (validate) {
    validate.parameter(template, hmm)
  } else {
    return(template)
  }
}
