#ifndef TNORM_H
#define TNORM_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

double dtnorm_cpp(double x, double mean, double sig, double point, bool above, bool log = false);

double dttnorm_cpp(double x, double mean, double sig, double lower, double upper, bool log = false);

double rtnorm_cpp(double mean, double sig, double point, bool above, bool log = false);

double rttnorm_cpp(double mean, double sig, double lower, double upper, bool log = false);

#endif
