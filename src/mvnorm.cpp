// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dmvnorm(
   arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
   bool log = false
) {
  double density = 0.0;
  if (arma::all(arma::vectorise(Sigma) == 0)) {
    // if Sigma = 0, degenerate distribution
    if (arma::all(x == mean)) {
      density = std::numeric_limits<double>::infinity();
    }
  } else {
    int p = x.size();
    arma::mat quadform = trans(x-mean) * solve(Sigma, arma::eye(p,p)) * (x-mean);
    double norm = pow(std::sqrt(2.0 * M_PI), -p) * pow(arma::det(Sigma), -0.5);
    density = norm * exp(-0.5 * quadform(0,0));
  }
  if (log) return std::log(density);
  return density;
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
