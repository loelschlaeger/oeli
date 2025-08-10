#' Gaussian total variation
#'
#' @description
#' Computes the total variation (TV) between two multivariate Gaussian
#' distributions \eqn{f_1,f_2}: \deqn{\mathrm{TV}(f_1, f_2) = \tfrac{1}{2}
#' \int_{\mathbb{R}^p} \lvert f_1(x) - f_2(x) \rvert \, dx.}
#' The value ranges from 0 (identical distributions) to 1 (no overlap).
#'
#' @param mean1,mean2 \[`numeric(p)`\]\cr
#' The mean vectors.
#'
#' @param Sigma1,Sigma2 \[`matrix(nrow = p, ncol = p)`\]\cr
#' The covariance matrices.
#'
#' @param method \[`character(1)`\]\cr
#' Computation method. One of:
#'
#' * `"auto"`: use closed-form formula when covariances are equal, otherwise
#'   use `"cubature"` for \eqn{p \le 2} and `"mc"` for higher dimensions.
#'
#' * `"mc"`: estimate via Monte Carlo importance sampling from the mixture
#'   \eqn{0.5 (f_1 + f_2)}.
#'
#' * `"cubature"`: compute overlap via adaptive cubature integration over a
#'   bounding box, then convert to TV. Exact but slow for \eqn{p \ge 2}.
#'
#' @param n \[`integer(1)`\]\cr
#' Number of Monte Carlo samples to draw.
#'
#' @param tol_equal \[`numeric(1)`\]\cr
#' Numerical tolerance used to decide whether `Sigma1` and `Sigma2` are
#' considered equal (enabling the closed-form formula in `"auto"` mode).
#'
#' @param eps \[`numeric(1)`\]\cr
#' Only used when `method = "cubature"`. Specifies the total probability mass
#' allowed to lie outside the integration hyper-rectangle across all dimensions.
#' This determines the numerical integration bounds: the function chooses limits
#' so that the probability of a point from either Gaussian falling outside the
#' box is at most `eps`. The bound is split evenly across dimensions via a union
#' bound, so the per-dimension tail probability is approximately `eps / p`.
#' Smaller values produce wider bounds (slower but more accurate integration),
#' while larger values yield narrower bounds (faster but potentially less
#' accurate).
#'
#' @return
#' The total variation in \[0, 1\].
#'
#' @keywords distribution
#' @family simulation helpers
#' @export
#'
#' @examples
#' ### univariate case
#' mean1 <- 0
#' mean2 <- 1
#' Sigma1 <- Sigma2 <- matrix(1)
#' gaussian_tv(mean1, mean2, Sigma1, Sigma2)
#'
#' ### bivariate case
#' mean1 <- c(0, 0)
#' mean2 <- c(1, 1)
#' Sigma1 <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
#' Sigma2 <- matrix(c(1.5, -0.3, -0.3, 1), ncol = 2)
#' gaussian_tv(mean1, mean2, Sigma1, Sigma2, method = "mc", n = 1e3)

gaussian_tv <- function(
  mean1, mean2, Sigma1, Sigma2, method = c("auto", "mc", "cubature"),
  n = 1e4, tol_equal = 1e-6, eps = 1e-6
) {
  method <- match.arg(method)
  input_check_response(
    check = check_numeric_vector(mean1, finite = TRUE, any.missing = FALSE),
    var_name = "mean1"
  )
  p <- length(mean1)
  input_check_response(
    check = check_numeric_vector(
      mean2, finite = TRUE, any.missing = FALSE, len = p
    ),
    var_name = "mean2"
  )
  if (p == 1) {
    if (checkmate::test_number(Sigma1)) {
      Sigma1 <- as.matrix(Sigma1)
    }
    if (checkmate::test_number(Sigma2)) {
      Sigma2 <- as.matrix(Sigma2)
    }
  }
  input_check_response(
    check = check_covariance_matrix(Sigma1, dim = p),
    var_name = "Sigma1"
  )
  input_check_response(
    check = check_covariance_matrix(Sigma2, dim = p),
    var_name = "Sigma2"
  )
  if (identical(mean1, mean2) && identical(Sigma1, Sigma2)) return(0)
  input_check_response(
    check = checkmate::check_number(tol_equal, lower = 0, finite = TRUE),
    var_name = "tol_equal"
  )
  cov_equal <- isTRUE(all.equal(Sigma1, Sigma2, tolerance = tol_equal))
  if (method == "auto" && cov_equal) {
    mu_diff <- mean1 - mean2
    R <- chol(Sigma1)
    z <- backsolve(R, forwardsolve(t(R), mu_diff))
    delta <- sqrt(sum(z * z))
    return(1 - 2 * pnorm(-delta / 2))
  }
  if (method == "cubature" || (method == "auto" && p <= 2)) {
    s1 <- sqrt(diag(Sigma1)); s2 <- sqrt(diag(Sigma2))
    k <- qnorm(1 - eps / (2 * p))
    lower <- pmin(mean1 - k * s1, mean2 - k * s2)
    upper <- pmax(mean1 + k * s1, mean2 + k * s2)
    min_f1_f2 <- function(x) {
      f1 <- dmvnorm(x, mean = mean1, Sigma = Sigma1)
      f2 <- dmvnorm(x, mean = mean2, Sigma = Sigma2)
      pmin(f1, f2)
    }
    ovl <- cubature::hcubature(f = min_f1_f2, lower, upper)$integral
    return(1 - ovl)
  }
  input_check_response(
    check = checkmate::check_int(n, lower = 2),
    var_name = "n"
  )
  n1 <- n %/% 2
  n2 <- n - n1
  X1 <- rmvnorm(n = 1, mean1, Sigma1)
  X1 <- replicate(n1, rmvnorm(n = 1, mean1, Sigma1), simplify = TRUE)
  X2 <- replicate(n2, rmvnorm(n = 1, mean2, Sigma2), simplify = TRUE)
  X  <- cbind(X1, X2)
  lf1 <- apply(X, 2, dmvnorm, mean = mean1, Sigma = Sigma1, log = TRUE)
  lf2 <- apply(X, 2, dmvnorm, mean = mean2, Sigma = Sigma2, log = TRUE)
  log_min <- pmin(lf1, lf2)
  m <- pmax(lf1, lf2)
  log_q <- m + log(exp(lf1 - m) + exp(lf2 - m)) - log(2)
  w <- exp(log_min - log_q)
  ovl_hat <- mean(w)
  tv_hat  <- 1 - ovl_hat
  se_ovl <- stats::sd(w) / sqrt(length(w))
  structure(tv_hat, "se" = se_ovl)
}

