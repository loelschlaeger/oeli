#ifndef OELI_TNORM_H
#define OELI_TNORM_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dtnorm(double x, double mean, double sig, double point, bool above, bool log = false);

double dttnorm(double x, double mean, double sig, double lower, double upper, bool log = false);

double rtnorm(double mean, double sig, double point, bool above, bool log = false);

double rttnorm(double mean, double sig, double lower, double upper, bool log = false);

#endif
