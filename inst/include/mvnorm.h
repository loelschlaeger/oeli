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
    double abseps = 1e-3,
    Rcpp::Nullable<Rcpp::NumericVector> lower = R_NilValue,
    std::string method = "genz", int draws = 500
);

arma::vec rmvnorm(
    arma::vec const& mean, arma::mat const& Sigma, bool log = false
);

#endif
