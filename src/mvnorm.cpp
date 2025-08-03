// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dmvnorm(
  arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
  bool log = false
) {
  int p = x.n_elem;
  arma::vec diff = x - mean;
  arma::mat L = arma::chol(Sigma, "lower");
  arma::vec v = arma::solve(arma::trimatl(L), diff);
  double quad = arma::dot(v, v);
  double log_det = 2.0 * arma::sum(arma::log(L.diag()));
  double log_density = -0.5 * (p * std::log(2.0 * M_PI) + log_det + quad);
  return log ? log_density : std::exp(log_density);
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

arma::vec rmvnorm(arma::vec mean, arma::mat const& Sigma, bool log = false) {
  int p = mean.size();
  arma::vec draw = arma::zeros<arma::vec>(p);
  if (arma::all(arma::vectorise(Sigma) == 0)) {
    // if Sigma = 0, just draw mean
    draw = mean;
  } else {
    arma::mat L = trans(chol(Sigma));
    arma::vec eps = Rcpp::rnorm(p, 0.0, 1.0);
    draw = L * eps + mean;
  }
  if (log) {
    for (int j = 0; j < p; ++j) {
      draw(j) = std::exp(draw(j));
    }
  }
  return draw;
}
