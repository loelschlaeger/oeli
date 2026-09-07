#ifndef OELI_TNORM_H
#define OELI_TNORM_H

// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

namespace oeli {

inline double dtnorm(
  double x, double mean, double sd, double point, bool above, bool log = false
) {
  const bool outside = above ? (x > point) : (x < point);
  if (outside) return log ? R_NegInf : 0.0;
  const double z = (x - mean) / sd;
  const double bound = (point - mean) / sd;
  const double log_mass = R::pnorm(bound, 0.0, 1.0, above ? 1 : 0, 1);
  const double log_density = R::dnorm(z, 0.0, 1.0, 1) - std::log(sd) - log_mass;
  return log ? log_density : std::exp(log_density);
}

inline double dttnorm(
  double x, double mean, double sd, double lower, double upper, bool log = false
) {
  if (x < lower || x > upper) return log ? R_NegInf : 0.0;
  const double z_lower = (lower - mean) / sd;
  const double z_upper = (upper - mean) / sd;
  double log_mass;
  if (z_lower + z_upper > 0.0) {
    const double log_large = R::pnorm(z_lower, 0.0, 1.0, 0, 1);
    const double log_small = R::pnorm(z_upper, 0.0, 1.0, 0, 1);
    log_mass = log_large + std::log1p(-std::exp(log_small - log_large));
  } else {
    const double log_large = R::pnorm(z_upper, 0.0, 1.0, 1, 1);
    const double log_small = R::pnorm(z_lower, 0.0, 1.0, 1, 1);
    log_mass = log_large + std::log1p(-std::exp(log_small - log_large));
  }
  const double z = (x - mean) / sd;
  const double log_density = R::dnorm(z, 0.0, 1.0, 1) - std::log(sd) - log_mass;
  return log ? log_density : std::exp(log_density);
}

// Draws from a standard normal distribution truncated below at `a` with the
// rejection methods of Robert (1995): normal proposals when the truncation
// point is negative and exponential proposals otherwise.
inline double rtnorm_standard(double a) {
  if (a < 0.0) {
    for (;;) {
      const double z = norm_rand();
      if (z > a) return z;
    }
  }
  const double rate = 0.5 * (a + std::sqrt(a * a + 4.0));
  for (;;) {
    const double z = a - std::log(unif_rand()) / rate;
    if (unif_rand() <= std::exp(-0.5 * (z - rate) * (z - rate))) return z;
  }
}

inline double rtnorm(
  double mean, double sd, double point, bool above, bool log = false
) {
  const double z = (point - mean) / sd;
  const double standard = above ? -rtnorm_standard(-z) : rtnorm_standard(z);
  const double draw = mean + sd * standard;
  return log ? std::exp(draw) : draw;
}

// The tail that contains the interval is inverted on the log-probability
// scale, so that neither bound probability rounds to zero or one.
inline double rttnorm(
  double mean, double sd, double lower, double upper, bool log = false
) {
  const double z_lower = (lower - mean) / sd;
  const double z_upper = (upper - mean) / sd;
  const double u = unif_rand();
  double draw;
  if (z_lower + z_upper > 0.0) {
    const double log_large = R::pnorm(z_lower, 0.0, 1.0, 0, 1);
    const double log_small = R::pnorm(z_upper, 0.0, 1.0, 0, 1);
    const double ratio = std::exp(log_small - log_large);
    const double log_p =
      log_large + std::log(ratio + (1.0 - u) * (1.0 - ratio));
    draw = mean + sd * R::qnorm(log_p, 0.0, 1.0, 0, 1);
  } else {
    const double log_large = R::pnorm(z_upper, 0.0, 1.0, 1, 1);
    const double log_small = R::pnorm(z_lower, 0.0, 1.0, 1, 1);
    const double ratio = std::exp(log_small - log_large);
    const double log_p = log_large + std::log(ratio + u * (1.0 - ratio));
    draw = mean + sd * R::qnorm(log_p, 0.0, 1.0, 1, 1);
  }
  return log ? std::exp(draw) : draw;
}

}

#endif
