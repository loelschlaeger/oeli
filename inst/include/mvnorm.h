#ifndef OELI_MVNORM_H
#define OELI_MVNORM_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dmvnorm(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    bool log = false
);

double pmvnorm(
    arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
    double abseps = 1e-3
);

arma::vec rmvnorm(arma::vec mean, arma::mat const& Sigma, bool log = false);

#endif
