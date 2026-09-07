#ifndef OELI_WISHART_H
#define OELI_WISHART_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

namespace oeli {

inline double dwishart(
  arma::mat const& x, double df, arma::mat const& scale, bool log = false,
  bool inv = false
) {
  const int p = static_cast<int>(x.n_cols);
  double log_gamma = 0.25 * p * (p - 1) * std::log(M_PI);
  for (int j = 1; j <= p; ++j) {
    log_gamma += std::lgamma(0.5 * (df - j + 1));
  }
  const double log_det_x = arma::log_det_sympd(x);
  const double log_det_scale = arma::log_det_sympd(scale);
  const double log_norm = 0.5 * df * p * std::log(2.0) + log_gamma;
  double log_density;
  if (inv) {
    log_density = 0.5 * df * log_det_scale -
      0.5 * arma::trace(scale * arma::inv_sympd(x)) -
      0.5 * (df + p + 1.0) * log_det_x - log_norm;
  } else {
    log_density = 0.5 * (df - p - 1.0) * log_det_x -
      0.5 * arma::trace(arma::inv_sympd(scale) * x) -
      0.5 * df * log_det_scale - log_norm;
  }
  return log ? log_density : std::exp(log_density);
}

inline arma::mat rwishart(double df, arma::mat const& scale, bool inv = false) {
  const int p = static_cast<int>(scale.n_rows);
  arma::mat T(p, p, arma::fill::zeros);
  for (int i = 0; i < p; ++i) {
    T(i, i) = std::sqrt(R::rchisq(df - i));
    for (int j = 0; j < i; ++j) {
      T(i, j) = norm_rand();
    }
  }
  const arma::mat L = arma::chol(inv ? arma::inv_sympd(scale) : scale, "lower");
  const arma::mat C = L * T;
  const arma::mat draw = C * C.t();
  return inv ? arma::inv_sympd(draw) : draw;
}

}

#endif
