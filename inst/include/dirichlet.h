#ifndef OELI_DIRICHLET_H
#define OELI_DIRICHLET_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

namespace oeli {

// The density is computed on the logarithmic scale, so that it remains
// accurate for large concentrations.
inline double ddirichlet(
  arma::vec const& x, arma::vec const& concentration, bool log = false
) {
  double log_density = std::lgamma(arma::accu(concentration));
  for (arma::uword j = 0; j < x.n_elem; ++j) {
    log_density -= std::lgamma(concentration[j]);
    if (concentration[j] != 1.0) {
      log_density += (concentration[j] - 1.0) * std::log(x[j]);
    }
  }
  return log ? log_density : std::exp(log_density);
}

inline arma::vec rdirichlet(arma::vec const& concentration) {
  const arma::uword p = concentration.n_elem;
  arma::vec draw(p);
  for (arma::uword j = 0; j < p; ++j) {
    draw[j] = R::rgamma(concentration[j], 1.0);
  }
  return draw / arma::accu(draw);
}

}

#endif
