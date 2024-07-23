#' Simulate correlated regressor values
#'
#' @description
#' This function simulates regressor values from various marginal distributions
#' with custom correlations.
#'
#' @param labels \[`character()`\]\cr
#' Unique labels for the regressors.
#'
#' @param n \[`integer(1)`\]\cr
#' The number of values per regressor.
#'
#' @param marginals \[`list()`\]\cr
#' Optionally marginal distributions for regressors. If not specified,
#' standard normal marginal distributions are used.
#'
#' Each list entry must be named according to a regressor label, and the
#' following distributions are currently supported:
#' \describe{
#'   \item{discrete distributions}{
#'     \itemize{
#'       \item Poisson: `list(type = "poisson", lambda = ...)`
#'       \item categorical: `list(type = "categorical", p = c(...))`
#'     }
#'   }
#'   \item{continuous distributions}{
#'     \itemize{
#'       \item normal: `list(type = "normal", mean = ..., sd = ...)`
#'       \item uniform: `list(type = "uniform", min = ..., max = ...)`
#'     }
#'   }
#' }
#'
#' @param correlation \[`matrix()`\]\cr
#' A correlation matrix of dimension \code{length(labels)}, where the
#' \code{(p, q)}-th entry defines the correlation between regressor
#' \code{labels[p]} and \code{labels[q]}.
#'
#' @param verbose \[`logical(1)`\]\cr
#' Print information about the simulated regressors?
#'
#' @return
#' A \code{data.frame} with \code{n} rows and \code{length(labels)} columns.
#'
#' @keywords simulation
#' @family simulation helpers
#' @export
#'
#' @references
#' This function heavily depends on the `{SimMultiCorrData}` package.
#'
#' @examples
#' labels <- c("P", "C", "N1", "N2", "U")
#' n <- 100
#' marginals <- list(
#'   "P" = list(type = "poisson", lambda = 2),
#'   "C" = list(type = "categorical", p = c(0.3, 0.2, 0.5)),
#'   "N1" = list(type = "normal", mean = -1, sd = 2),
#'   "U" = list(type = "uniform", min = -2, max = -1)
#' )
#' correlation <- matrix(
#'   c(1, -0.3, -0.1, 0, 0.5,
#'     -0.3, 1, 0.3, -0.5, -0.7,
#'     -0.1, 0.3, 1, -0.3, -0.3,
#'     0, -0.5, -0.3, 1, 0.1,
#'     0.5, -0.7, -0.3, 0.1, 1),
#'   nrow = 5, ncol = 5
#' )
#' data <- correlated_regressors(
#'   labels = labels, n = n, marginals = marginals, correlation = correlation
#' )
#' head(data)

correlated_regressors <- function(
    labels,
    n = 100,
    marginals = list(),
    correlation = diag(length(labels)),
    verbose = FALSE
  ) {

  ### input checks
  input_check_response(
    checkmate::check_character(
      labels, min.chars = 1, any.missing = FALSE, min.len = 1, unique = TRUE
    ),
    "labels"
  )
  input_check_response(
    checkmate::check_count(n, positive = TRUE),
    "n"
  )
  P <- length(labels)
  input_check_response(
    checkmate::check_list(
      marginals, types = list(), any.missing = FALSE, max.len = P,
      names = "strict"
    ),
    "marginals"
  )
  input_check_response(
    checkmate::check_subset(
      names(marginals), choices = labels, empty.ok = TRUE
    ),
    "marginals",
    prefix = "Element names of input {var_name} are bad:"
  )
  input_check_response(
    check_correlation_matrix(
      correlation, dim = P
    ),
    "correlation"
  )
  correlation_req <- correlation
  input_check_response(
    checkmate::check_flag(verbose),
    "verbose"
  )

  ### draw data
  if (length(marginals) == 0) {

    ### no marginals specified, so draw from multivariate normal
    data <- rmvnorm(n = n, rep(0, P), correlation)
    if (n == 1) {
      data <- matrix(data, nrow = 1)
    }
    colnames(data) <- labels
    marginals_info <- rep("Normal(mean = 0, sd = 1)", P)

  } else {

    ### interpret marginal distributions
    marginals_class <- character(P)
    marginals_info <- character(P)
    M <- matrix(NA, nrow = 6, ncol = 0)
    lam <- numeric()
    marginal <- list()
    size <- numeric()
    prob <- numeric()

    for (p in seq_len(P)) {

      if (labels[p] %in% names(marginals)) {

        ### check structure
        input_check_response(
          checkmate::check_list(
            marginals[labels[p]], names = "strict", any.missing = FALSE
          ),
          "marginals",
          prefix = paste0("Element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
        )
        type <- marginals[[labels[p]]][["type"]]
        input_check_response(
          checkmate::check_choice(
            type,
            c("poisson", "categorical", "normal", "uniform")
          ),
          "marginals",
          prefix = paste0("Element {.val type} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
        )
        if (type == "poisson") {

          lam <- c(lam, marginals[[labels[p]]][["lambda"]])
          input_check_response(
            checkmate::check_number(lam, lower = 0, finite = TRUE),
            "marginals",
            prefix = paste0("Element {.val lambda} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          marginals_class[p] <- "pois"
          marginals_info[p] <- paste0("Poisson(lambda = ", lam, ")")

        } else if (type == "categorical") {

          cat_prob_p <- marginals[[labels[p]]][["p"]]
          input_check_response(
            check_probability_vector(cat_prob_p),
            "marginals",
            prefix = paste0("Element {.val p} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          marginal <- c(marginal, list(cumsum(cat_prob_p)[-length(cat_prob_p)]))
          marginals_class[p] <- "cat"
          marginals_info[p] <- paste0(
            "Categorical(p = ", paste(cat_prob_p, collapse = ",") , ")"
          )

        } else if (type == "normal") {

          normal_mean_p <- marginals[[labels[p]]][["mean"]]
          input_check_response(
            checkmate::check_number(normal_mean_p, finite = TRUE),
            "marginals",
            prefix = paste0("Element {.val mean} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          normal_sd_p <- marginals[[labels[p]]][["sd"]]
          input_check_response(
            checkmate::check_number(normal_sd_p, lower = 0, finite = TRUE),
            "marginals",
            prefix = paste0("Element {.val sd} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          M <- cbind(M, SimMultiCorrData::calc_theory(
            Dist = "Gaussian",
            params = c(normal_mean_p, normal_sd_p)
          ))
          marginals_class[p] <- "cont"
          marginals_info[p] <- paste0(
            "Normal(mean = ", normal_mean_p, ", sd = ", normal_sd_p, ")"
          )

        } else if (type == "uniform") {

          uniform_min_p <- marginals[[labels[p]]][["min"]]
          input_check_response(
            checkmate::check_number(uniform_min_p, finite = TRUE),
            "marginals",
            prefix = paste0("Element {.val min} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          uniform_max_p <- marginals[[labels[p]]][["max"]]
          input_check_response(
            checkmate::check_number(uniform_max_p, lower = uniform_min_p, finite = TRUE),
            "marginals",
            prefix = paste0("Element {.val max} of element {.val ", labels[p], "} in input {.var {var_name}} is bad:")
          )
          M <- cbind(M, SimMultiCorrData::calc_theory(
            Dist = "Uniform",
            params = c(uniform_min_p, uniform_max_p)
          ))
          marginals_class[p] <- "cont"
          marginals_info[p] <- paste0(
            "Uniform(min = ", uniform_min_p, ", max = ", uniform_max_p, ")"
          )

        } else {
          unexpected_error()
        }


      } else {

        M <- cbind(M, SimMultiCorrData::calc_theory(
          Dist = "Gaussian", params = c(0, 1)
        ))
        marginals_class[p] <- "cont"
        marginals_info[p] <- "Normal(mean = 0, sd = 1)"

      }

    }

    ### adapt order of correlation
    marginals_class <- factor(marginals_class, levels = c("cat", "cont", "pois"), ordered = TRUE)
    marginals_order <- order(marginals_class)
    ordering_operator <- diag(P)[marginals_order, ]
    correlation <- ordering_operator %*% correlation %*% t(ordering_operator)

    ### make sure 'correlation' is within upper and lower correlation limits
    valid <- try_silent(quiet(SimMultiCorrData::valid_corr(
      k_cat = sum(marginals_class == "cat"),
      k_cont = sum(marginals_class == "cont"),
      k_pois = sum(marginals_class == "pois"),
      k_nb = sum(marginals_class == "nb"),
      method = "Polynomial",
      means =  M[1, ],
      vars =  (M[2, ])^2,
      skews = M[3, ],
      skurts = M[4, ],
      fifths = M[5, ],
      sixths = M[6, ],
      marginal = marginal,
      lam = lam,
      size = size,
      prob = prob,
      rho = correlation,
      seed = NULL
    )))

    ### check for failure
    if (inherits(valid, "fail")) {
      cli::cli_abort(
        "Unable to fit {.var correlation} with {.var marginals}, please check specification.",
        call = NULL
      )
    }

    ### report info about correlation limits
    for (i in 1:(P - 1)) {
      for (j in (i + 1):P) {
        if (correlation[i, j] < valid$L_rho[i, j]) {
          cli::cli_warn(
            "{.code correlation[{i},{j}] = {correlation[i,j]}} not possible
            (too small), increased to {valid$L_rho[i,j]}"
          )
          correlation[i, j] <- correlation[j, i] <- valid$L_rho[i, j]
        }
        if (correlation[i, j] > valid$U_rho[i, j]) {
          cli::cli_warn(
            "{.code correlation[{i},{j}] = {correlation[i,j]}} not possible
            (too large), decreased to {valid$U_rho[i,j]}"
          )
          correlation[i, j] <- correlation[j, i] <- valid$U_rho[i, j]
        }
      }
    }

    ### simulate data from marginal distributions
    sim_out <- quiet(SimMultiCorrData::rcorrvar(
      n = n,
      k_cat = sum(marginals_class == "cat"),
      k_cont = sum(marginals_class == "cont"),
      k_pois = sum(marginals_class == "pois"),
      k_nb = sum(marginals_class == "nb"),
      method = "Polynomial",
      means =  M[1, ],
      vars =  (M[2, ])^2,
      skews = M[3, ],
      skurts = M[4, ],
      fifths = M[5, ],
      sixths = M[6, ],
      marginal = marginal,
      lam = lam,
      size = size,
      prob = prob,
      rho = correlation,
      seed = NULL
    ))

    #### obtain regressors
    data <- cbind(
      sim_out$ordinal_variables,
      sim_out$continuous_variables,
      sim_out$Poisson_variables
    )

    ### adapt order of regressors and add column names
    data <- data[, order(marginals_order)]
    colnames(data) <- labels
  }

  ### print status and return data
  if (isTRUE(verbose)) {
    cat("Marginal distributions for regressors:\n")
    for (p in seq_len(P)) {
      cat("-", labels[p], "~", marginals_info[p], "\n")
    }
    if (n > 1) {
      cat("\nDeviation of empirical correlations from requested:\n")
      print(round(summary(as.numeric(stats::cor(data) - correlation_req)), 4))
    }
  }
  as.data.frame(data)
}





