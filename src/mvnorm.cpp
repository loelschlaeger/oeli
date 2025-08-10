// [[Rcpp::depends(RcppArmadillo, mvtnorm)]]
#include <RcppArmadillo.h>
#include <mvtnormAPI.h>

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

double pmvnorm(
  arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
  double abseps = 1e-3
) {
  int n = x.n_elem;
  arma::vec sds = arma::sqrt(Sigma.diag());
  arma::mat corrmat = arma::diagmat(1.0 / sds) * Sigma * arma::diagmat(1.0 / sds);
  arma::vec bound = (x - mean) / sds;
  arma::vec lowertrivec(n * (n - 1) / 2);
  int k = 0;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < i; ++j) {
      lowertrivec(k++) = corrmat(i, j);
    }
  }
  int nu = 0;
  int maxpts = 25000;
  double releps = 0;
  int rnd = 1;
  double* bound_ = bound.memptr();
  double* correlationMatrix = lowertrivec.memptr();
  double* lower = new double[n];
  int* infin = new int[n];
  double* delta = new double[n];
  for (int i = 0; i < n; ++i) {
    infin[i] = 0;
    lower[i] = 0.0;
    delta[i] = 0.0;
  }
  double error;
  double value;
  int inform;
  mvtnorm_C_mvtdst(
    &n, &nu, lower, bound_, infin, correlationMatrix, delta, &maxpts, &abseps,
    &releps, &error, &value, &inform, &rnd
  );
  delete[] (lower);
  delete[] (infin);
  delete[] (delta);
  return value;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

arma::vec rmvnorm(
  arma::vec const& mean, arma::mat const& Sigma, bool log = false
) {
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
