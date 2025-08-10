#ifndef OELI_MIXNORM_H
#define OELI_MIXNORM_H

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mvnorm.h"

double dmixnorm(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions
);

double pmixnorm(
    arma::vec const& x, arma::mat const& mean, arma::mat const& Sigma,
    arma::vec proportions, double abseps = 1e-3
);

arma::vec rmixnorm(
    arma::mat const& mean, arma::mat const& Sigma, arma::vec proportions
);

#endif
