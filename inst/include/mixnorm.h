#ifndef OELI_MIXNORM_H
#define OELI_MIXNORM_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mvnorm.h"

namespace oeli {

// The proportions are rescaled to sum to one.
inline arma::vec mixture_proportions(arma::vec proportions) {
  const double total = arma::accu(proportions);
  if (total > 0.0) proportions /= total;
  return proportions;
}

inline double dmixnorm(
  arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
  arma::vec proportions, bool log = false
) {
  const int p = static_cast<int>(x.n_elem);
  const int K = static_cast<int>(mean.n_cols);
  proportions = mixture_proportions(proportions);
  arma::vec log_terms(K);
  for (int k = 0; k < K; ++k) {
    const arma::mat Sigma_k = arma::reshape(Sigma.col(k), p, p);
    log_terms[k] = std::log(proportions[k]) +
      dmvnorm(x, mean.col(k), Sigma_k, true);
  }
  const double maximum = log_terms.max();
  if (!std::isfinite(maximum)) return log ? R_NegInf : 0.0;
  const double log_density =
    maximum + std::log(arma::accu(arma::exp(log_terms - maximum)));
  return log ? log_density : std::exp(log_density);
}

inline double pmixnorm(
  arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
  arma::vec proportions, double abseps = 1e-3,
  Rcpp::Nullable<Rcpp::NumericVector> lower = R_NilValue,
  std::string method = "genz", int draws = 500
) {
  const int p = static_cast<int>(x.n_elem);
  const int K = static_cast<int>(mean.n_cols);
  proportions = mixture_proportions(proportions);
  double probability = 0.0;
  for (int k = 0; k < K; ++k) {
    const arma::mat Sigma_k = arma::reshape(Sigma.col(k), p, p);
    probability += proportions[k] *
      pmvnorm(x, mean.col(k), Sigma_k, abseps, lower, method, draws);
  }
  return probability;
}

inline arma::vec rmixnorm(
  arma::mat const& mean, arma::mat const& Sigma, arma::vec proportions,
  bool log = false
) {
  const int p = static_cast<int>(mean.n_rows);
  const int K = static_cast<int>(mean.n_cols);
  proportions = mixture_proportions(proportions);
  const double u = unif_rand();
  int k = 0;
  double cumulative = proportions[0];
  while (u > cumulative && k < K - 1) {
    ++k;
    cumulative += proportions[k];
  }
  const arma::mat Sigma_k = arma::reshape(Sigma.col(k), p, p);
  return rmvnorm(mean.col(k), Sigma_k, log);
}

}

#endif
