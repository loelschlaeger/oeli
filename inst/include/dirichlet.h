#ifndef OELI_DIRICHLET_H
#define OELI_DIRICHLET_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double ddirichlet(arma::vec const& x, arma::vec const& concentration, bool log = false);

arma::vec rdirichlet(arma::vec const& concentration);

#endif
