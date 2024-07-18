#include <testthat.h>
#include "dirichlet.h"

context("dirichlet") {

  test_that("Dirichlet density can be computed") {
    arma::vec x = arma::zeros<arma::vec>(3);
    x(0) = 0.5; x(1) = 0.3; x(2) = 0.2;
    arma::vec concentration = arma::zeros<arma::vec>(3);
    concentration(0) = 1; concentration(1) = 2; concentration(2) = 3;
    double density = ddirichlet_cpp(x, concentration, false);
    int factor = 1000;
    double density_rounded = round(density * factor) / factor;
    expect_true(density_rounded == 0.720);
    double density_log = ddirichlet_cpp(x, concentration, true);
    double density_log_rounded = round(density_log * factor) / factor;
    expect_true(density_log_rounded == -0.329);
  }

  test_that("Dirichlet can be drawn") {
    arma::vec concentration = arma::zeros<arma::vec>(3);
    concentration(0) = 1; concentration(1) = 2; concentration(2) = 3;
    arma::vec draw = rdirichlet_cpp(concentration);
    expect_true(draw.size() == 3);
    expect_true(abs(sum(draw) - 1) < 0.001);
  }

}
