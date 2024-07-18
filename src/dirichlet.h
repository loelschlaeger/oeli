#ifndef DIRICHLET_H
#define DIRICHLET_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double ddirichlet_cpp(arma::vec const& x, arma::vec const& concentration, bool log = false);

arma::vec rdirichlet_cpp(arma::vec const& concentration);

#endif
