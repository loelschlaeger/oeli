// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "../inst/include/mvnorm.h"

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dmixnorm(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions
) {
  int p = x.n_elem;
  int K = mean.n_cols;
  double out = 0.0;
  double sum_props = arma::accu(proportions);
  if (std::abs(sum_props - 1.0) > 1e-12 && sum_props > 0) {
    proportions /= sum_props;
  }
  for (int k = 0; k < K; ++k) {
    arma::mat Sk = arma::reshape(Sigma.col(k), p, p);
    out += proportions(k) * dmvnorm(x, mean.col(k), Sk, false);
  }
  return out;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double pmixnorm(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions, double abseps = 1e-3
) {
  int p = x.n_elem;
  int K = mean.n_cols;
  double out = 0.0;
  double sum_props = arma::accu(proportions);
  if (std::abs(sum_props - 1.0) > 1e-12 && sum_props > 0) {
    proportions /= sum_props;
  }
  for (int k = 0; k < K; ++k) {
    arma::mat Sk = arma::reshape(Sigma.col(k), p, p);
    out += proportions(k) * pmvnorm(x, mean.col(k), Sk);
  }
  return out;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

arma::vec rmixnorm(
    arma::mat const& mean, arma::mat const& Sigma, arma::vec proportions
) {
  int p = mean.n_rows;
  int K = mean.n_cols;
  double sum_props = arma::accu(proportions);
  if (std::abs(sum_props - 1.0) > 1e-12 && sum_props > 0) {
    proportions /= sum_props;
  }
  double u = R::runif(0.0, 1.0);
  int k = 0;
  double csum = proportions(0);
  while (u > csum && k < K - 1) {
    ++k;
    csum += proportions(k);
  }
  arma::mat Sk = arma::reshape(Sigma.col(k), p, p);
  return rmvnorm(mean.col(k), Sk, false);
}

