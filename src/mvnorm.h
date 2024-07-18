#ifndef MVNORM_H
#define MVNORM_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dmvnorm_cpp(arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma, bool log = false);

arma::vec rmvnorm_cpp(arma::vec mean, arma::mat const& Sigma, bool log = false);

#endif
