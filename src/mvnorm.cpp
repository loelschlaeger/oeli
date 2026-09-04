// [[Rcpp::depends(RcppArmadillo, mvtnorm)]]
#include <RcppArmadillo.h>
#include <mvtnormAPI.h>
#include <random>

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double dmvnorm(
  arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
  bool log = false
) {
  int p = x.n_elem;
  arma::vec diff = x - mean;
  arma::mat L = arma::chol(Sigma, "lower");
  arma::vec v = arma::solve(arma::trimatl(L), diff);
  double quad = arma::dot(v, v);
  double log_det = 2.0 * arma::sum(arma::log(L.diag()));
  double log_density = -0.5 * (p * std::log(2.0 * M_PI) + log_det + quad);
  return log ? log_density : std::exp(log_density);
}

namespace {

const double negative_infinity = -std::numeric_limits<double>::infinity();

double phi(double x) {
  return R::pnorm(x, 0.0, 1.0, 1, 0);
}

// Gauss-Legendre nodes and weights used by Genz (2004)
const double gl_x[3][10] = {
  {-0.9324695142031522, -0.6612093864662647, -0.2386191860831970,
   0, 0, 0, 0, 0, 0, 0},
  {-0.9815606342467191, -0.9041172563704750, -0.7699026741943050,
   -0.5873179542866171, -0.3678314989981802, -0.1252334085114692,
   0, 0, 0, 0},
  {-0.9931285991850949, -0.9639719272779138, -0.9122344282513259,
   -0.8391169718222188, -0.7463319064601508, -0.6360536807265150,
   -0.5108670019508271, -0.3737060887154196, -0.2277858511416451,
   -0.07652652113349733}
};

const double gl_w[3][10] = {
  {0.1713244923791705, 0.3607615730481384, 0.4679139345726904,
   0, 0, 0, 0, 0, 0, 0},
  {0.04717533638651177, 0.1069393259953183, 0.1600783285433464,
   0.2031674267230659, 0.2334925365383547, 0.2491470458134029,
   0, 0, 0, 0},
  {0.01761400713915212, 0.04060142980038694, 0.06267204833410906,
   0.08327674157670475, 0.1019301198172404, 0.1181945319615184,
   0.1316886384491766, 0.1420961093183821, 0.1491729864726037,
   0.1527533871307259}
};

const int gl_n[3] = {3, 6, 10};

// upper orthant probability P(X > h, Y > k) of the standard bivariate normal
// distribution with correlation r, Genz (2004)
double bvn_upper(double h, double k, double r) {
  int ng = std::abs(r) < 0.3 ? 0 : (std::abs(r) < 0.75 ? 1 : 2);
  int lg = gl_n[ng];
  double hk = h * k;
  double bvn = 0.0;
  if (std::abs(r) < 0.925) {
    if (std::abs(r) > 0.0) {
      double hs = (h * h + k * k) / 2.0;
      double asr = std::asin(r);
      for (int i = 0; i < lg; ++i) {
        for (int is = -1; is <= 1; is += 2) {
          double sn = std::sin(asr * (is * gl_x[ng][i] + 1.0) / 2.0);
          bvn += gl_w[ng][i] * std::exp((sn * hk - hs) / (1.0 - sn * sn));
        }
      }
      bvn = bvn * asr / (4.0 * M_PI);
    }
    return bvn + phi(-h) * phi(-k);
  }
  if (r < 0.0) {
    k = -k;
    hk = -hk;
  }
  if (std::abs(r) < 1.0) {
    double as = (1.0 - r) * (1.0 + r);
    double a = std::sqrt(as);
    double bs = (h - k) * (h - k);
    double c = (4.0 - hk) / 8.0;
    double d = (12.0 - hk) / 16.0;
    double asr = -(bs / as + hk) / 2.0;
    if (asr > -100.0) {
      bvn = a * std::exp(asr) *
        (1.0 - c * (bs - as) * (1.0 - d * bs / 5.0) / 3.0 +
          c * d * as * as / 5.0);
    }
    if (-hk < 100.0) {
      double b = std::sqrt(bs);
      bvn -= std::exp(-hk / 2.0) * std::sqrt(2.0 * M_PI) * phi(-b / a) * b *
        (1.0 - c * bs * (1.0 - d * bs / 5.0) / 3.0);
    }
    a /= 2.0;
    for (int i = 0; i < lg; ++i) {
      for (int is = -1; is <= 1; is += 2) {
        double xs = std::pow(a * (is * gl_x[ng][i] + 1.0), 2);
        double rs = std::sqrt(1.0 - xs);
        asr = -(bs / xs + hk) / 2.0;
        if (asr > -100.0) {
          bvn += a * gl_w[ng][i] * std::exp(asr) *
            (std::exp(-hk * xs / (2.0 * (1.0 + rs) * (1.0 + rs))) / rs -
              (1.0 + c * xs * (1.0 + d * xs)));
        }
      }
    }
    bvn = -bvn / (2.0 * M_PI);
  }
  if (r > 0.0) {
    bvn += phi(-std::max(h, k));
  } else {
    bvn = -bvn;
    if (k > h) {
      bvn += h < 0.0 ? phi(k) - phi(h) : phi(-h) - phi(-k);
    }
  }
  return std::min(std::max(bvn, 0.0), 1.0);
}

double bvn_upper_extended(double h, double k, double r) {
  double inf = std::numeric_limits<double>::infinity();
  if (h == inf || k == inf) return 0.0;
  if (h == -inf && k == -inf) return 1.0;
  if (h == -inf) return phi(-k);
  if (k == -inf) return phi(-h);
  return bvn_upper(h, k, r);
}

// lower tail probability P(X1 < h1, X2 < h2, X3 < h3) of the standard
// trivariate normal distribution by conditioning on X3 and integrating the
// bivariate probability with an adaptive Gauss-Kronrod rule
struct tvn_integrand {
  double h1, h2, r, s1, s2, r13, r23;
  double operator()(double z) const {
    double a1 = (h1 - r13 * z) / s1;
    double a2 = (h2 - r23 * z) / s2;
    return R::dnorm(z, 0.0, 1.0, 0) * bvn_upper(-a1, -a2, r);
  }
};

double gauss_kronrod(
    tvn_integrand const& f, double a, double b, double tolerance, int depth
) {
  static const double xgk[8] = {
    0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
    0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
    0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
    0.207784955007898467600689403773245, 0.000000000000000000000000000000000
  };
  static const double wgk[8] = {
    0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
    0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
    0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
    0.204432940075298892414161999234649, 0.209482141084727828012999174891714
  };
  static const double wg[4] = {
    0.129484966168869693270611432679082, 0.279705391489276667901467771423780,
    0.381830050505118944950369775488975, 0.417959183673469387755102040816327
  };
  double center = (a + b) / 2.0;
  double half = (b - a) / 2.0;
  double fc = f(center);
  double kronrod = fc * wgk[7];
  double gauss = fc * wg[3];
  for (int j = 0; j < 7; ++j) {
    double dx = half * xgk[j];
    double fsum = f(center - dx) + f(center + dx);
    kronrod += wgk[j] * fsum;
    if (j % 2 == 1) gauss += wg[j / 2] * fsum;
  }
  kronrod *= half;
  gauss *= half;
  if (depth <= 0 || std::abs(kronrod - gauss) <= tolerance) return kronrod;
  return gauss_kronrod(f, a, center, tolerance / 2.0, depth - 1) +
    gauss_kronrod(f, center, b, tolerance / 2.0, depth - 1);
}

double tvn_lower(
    double h1, double h2, double h3, double r12, double r13, double r23
) {
  double inf = std::numeric_limits<double>::infinity();
  if (h1 == -inf || h2 == -inf || h3 == -inf) return 0.0;
  if (h3 == inf) {
    return bvn_upper_extended(-h1, -h2, r12);
  }
  double s1 = std::sqrt(std::max(1.0 - r13 * r13, 0.0));
  double s2 = std::sqrt(std::max(1.0 - r23 * r23, 0.0));
  if (s1 < 1e-6 || s2 < 1e-6) return NA_REAL;
  tvn_integrand f;
  f.h1 = h1 == inf ? 1e6 : h1;
  f.h2 = h2 == inf ? 1e6 : h2;
  f.r = std::min(std::max((r12 - r13 * r23) / (s1 * s2), -1.0), 1.0);
  f.s1 = s1;
  f.s2 = s2;
  f.r13 = r13;
  f.r23 = r23;
  double lower = -8.5;
  double upper = std::min(h3, 8.5);
  if (upper <= lower) return 0.0;
  double value = 0.0;
  double a = lower;
  double knots[2] = {std::min(std::max(0.0, lower), upper), upper};
  for (double b : knots) {
    if (b > a) {
      value += gauss_kronrod(f, a, b, 1e-12, 12);
      a = b;
    }
  }
  return std::min(std::max(value, 0.0), 1.0);
}

// rectangle probability of the standard trivariate normal distribution by
// inclusion-exclusion over the lower limits
double tvn_rectangle(
    arma::vec const& lower, arma::vec const& upper, arma::mat const& corr
) {
  double value = 0.0;
  for (int mask = 0; mask < 8; ++mask) {
    double bounds[3];
    int sign = 1;
    for (int i = 0; i < 3; ++i) {
      if (mask & (1 << i)) {
        if (lower[i] == negative_infinity) {
          sign = 0;
          break;
        }
        bounds[i] = lower[i];
        sign = -sign;
      } else {
        bounds[i] = upper[i];
      }
    }
    if (sign == 0) continue;
    double term = tvn_lower(
      bounds[0], bounds[1], bounds[2], corr(0, 1), corr(0, 2), corr(1, 2)
    );
    if (ISNAN(term)) return NA_REAL;
    value += sign * term;
  }
  return std::min(std::max(value, 0.0), 1.0);
}

// Halton points with a fixed random shift, cached across calls
arma::mat const& halton_points(int draws, int dims) {
  static arma::mat cache;
  static std::vector<int> primes;
  if (static_cast<int>(cache.n_rows) >= draws &&
      static_cast<int>(cache.n_cols) >= dims) {
    return cache;
  }
  int rows = std::max(draws, static_cast<int>(cache.n_rows));
  int cols = std::max(dims, static_cast<int>(cache.n_cols));
  while (static_cast<int>(primes.size()) < cols) {
    int candidate = primes.empty() ? 2 : primes.back() + 1;
    while (true) {
      bool prime = true;
      for (int p : primes) {
        if (p * p > candidate) break;
        if (candidate % p == 0) {
          prime = false;
          break;
        }
      }
      if (prime) break;
      ++candidate;
    }
    primes.push_back(candidate);
  }
  std::mt19937_64 rng(20240903ULL);
  std::uniform_real_distribution<double> unif(0.0, 1.0);
  cache.set_size(rows, cols);
  int skip = 100;
  for (int j = 0; j < cols; ++j) {
    double shift = unif(rng);
    int base = primes[j];
    for (int i = 0; i < rows; ++i) {
      int n = i + skip;
      double value = 0.0;
      double factor = 1.0 / base;
      while (n > 0) {
        value += factor * (n % base);
        n /= base;
        factor /= base;
      }
      value += shift;
      if (value >= 1.0) value -= 1.0;
      cache(i, j) = std::min(std::max(value, 1e-12), 1.0 - 1e-12);
    }
  }
  return cache;
}

// Geweke-Hajivassiliou-Keane simulator on quasi-random Halton points
double ghk(
    arma::vec const& lower, arma::vec const& upper, arma::mat const& corr,
    int draws
) {
  arma::uword p = upper.n_elem;
  arma::mat L;
  arma::mat C = arma::symmatu(corr);
  if (!arma::chol(L, C, "lower")) {
    C.diag() += 1e-8;
    if (!arma::chol(L, C, "lower")) return NA_REAL;
  }
  draws = std::max(draws, 2);
  arma::mat const& u = halton_points(draws, static_cast<int>(p));
  double maximum = negative_infinity;
  std::vector<double> log_weights(draws, negative_infinity);
  arma::vec e(p);
  for (int s = 0; s < draws; ++s) {
    double log_weight = 0.0;
    bool empty = false;
    for (arma::uword i = 0; i < p; ++i) {
      double shift = 0.0;
      for (arma::uword j = 0; j < i; ++j) shift += L(i, j) * e[j];
      double a = (lower[i] - shift) / L(i, i);
      double b = (upper[i] - shift) / L(i, i);
      bool upper_tail = a > 0.0;
      double Fa = R::pnorm(a, 0.0, 1.0, !upper_tail, 0);
      double Fb = R::pnorm(b, 0.0, 1.0, !upper_tail, 0);
      double mass = upper_tail ? (Fa - Fb) : (Fb - Fa);
      if (!(mass > 0.0)) {
        empty = true;
        break;
      }
      log_weight += std::log(mass);
      double v = u(s, i);
      double q = upper_tail ? (Fa - v * mass) : (Fa + v * mass);
      q = std::min(std::max(q, 1e-300), 1.0 - 1e-16);
      e[i] = R::qnorm(q, 0.0, 1.0, !upper_tail, 0);
    }
    if (!empty) {
      log_weights[s] = log_weight;
      maximum = std::max(maximum, log_weight);
    }
  }
  if (!std::isfinite(maximum)) return 0.0;
  double total = 0.0;
  for (double value : log_weights) total += std::exp(value - maximum);
  return std::exp(maximum + std::log(total / draws));
}

// randomized Quasi-Monte-Carlo integration by Genz and Bretz via mvtnorm
double genz(
    arma::vec const& lower, arma::vec const& upper, arma::mat const& corr,
    double abseps
) {
  int n = upper.n_elem;
  double inf = std::numeric_limits<double>::infinity();
  arma::vec lowertrivec(n * (n - 1) / 2);
  int k = 0;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < i; ++j) {
      lowertrivec(k++) = corr(i, j);
    }
  }
  std::vector<double> lower_(n), upper_(n), delta(n, 0.0);
  std::vector<int> infin(n);
  for (int i = 0; i < n; ++i) {
    bool finite_lower = lower[i] > -inf;
    bool finite_upper = upper[i] < inf;
    lower_[i] = finite_lower ? lower[i] : 0.0;
    upper_[i] = finite_upper ? upper[i] : 0.0;
    infin[i] = finite_lower ? (finite_upper ? 2 : 1) : (finite_upper ? 0 : -1);
  }
  int nu = 0;
  int maxpts = 25000;
  double releps = 0;
  int rnd = 1;
  double error;
  double value;
  int inform;
  mvtnorm_C_mvtdst(
    &n, &nu, lower_.data(), upper_.data(), infin.data(), lowertrivec.memptr(),
    delta.data(), &maxpts, &abseps, &releps, &error, &value, &inform, &rnd
  );
  return value;
}

}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

double pmvnorm(
  arma::vec const& x, arma::vec const& mean, arma::mat const& Sigma,
  double abseps = 1e-3, Rcpp::Nullable<Rcpp::NumericVector> lower = R_NilValue,
  std::string method = "genz", int draws = 500
) {
  int p = x.n_elem;
  arma::vec sds = arma::sqrt(Sigma.diag());
  arma::mat corr = arma::diagmat(1.0 / sds) * Sigma * arma::diagmat(1.0 / sds);
  arma::vec upper_std = (x - mean) / sds;
  arma::vec lower_std(p);
  lower_std.fill(negative_infinity);
  if (lower.isNotNull()) {
    Rcpp::NumericVector lower_vec(lower);
    for (int i = 0; i < p; ++i) {
      double value = lower_vec.size() == 1 ? lower_vec[0] : lower_vec[i];
      lower_std[i] = (value - mean[i]) / sds[i];
    }
  }
  for (int i = 0; i < p; ++i) {
    if (!(upper_std[i] > lower_std[i])) return 0.0;
  }
  if (p == 1) {
    bool upper_tail = lower_std[0] > 0.0;
    double Fa = R::pnorm(lower_std[0], 0.0, 1.0, !upper_tail, 0);
    double Fb = R::pnorm(upper_std[0], 0.0, 1.0, !upper_tail, 0);
    return std::max(upper_tail ? Fa - Fb : Fb - Fa, 0.0);
  }
  if (p == 2) {
    double r = std::min(std::max(corr(0, 1), -1.0), 1.0);
    double value =
      bvn_upper_extended(lower_std[0], lower_std[1], r) -
      bvn_upper_extended(upper_std[0], lower_std[1], r) -
      bvn_upper_extended(lower_std[0], upper_std[1], r) +
      bvn_upper_extended(upper_std[0], upper_std[1], r);
    return std::min(std::max(value, 0.0), 1.0);
  }
  if (p == 3) {
    double value = tvn_rectangle(lower_std, upper_std, corr);
    if (!ISNAN(value)) return value;
  }
  if (method == "ghk") return ghk(lower_std, upper_std, corr, draws);
  return genz(lower_std, upper_std, corr, abseps);
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]

arma::vec rmvnorm(
  arma::vec const& mean, arma::mat const& Sigma, bool log = false
) {
  int p = mean.size();
  arma::vec draw = arma::zeros<arma::vec>(p);
  if (arma::all(arma::vectorise(Sigma) == 0)) {
    // if Sigma = 0, just draw mean
    draw = mean;
  } else {
    arma::mat L = trans(chol(Sigma));
    arma::vec eps = Rcpp::rnorm(p, 0.0, 1.0);
    draw = L * eps + mean;
  }
  if (log) {
    for (int j = 0; j < p; ++j) {
      draw(j) = std::exp(draw(j));
    }
  }
  return draw;
}
