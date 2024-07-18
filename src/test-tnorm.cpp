#include <testthat.h>
#include "tnorm.h"

context("tnorm") {

  test_that("Univariate truncated normal can be drawn") {
    double mean = 0.0;
    double sd = 1.0;
    double point = 0.0;
    double draw_tb = rtnorm_cpp(mean, sd, point, false, false);
    expect_true(draw_tb > 0.0);
    double draw_ta = rtnorm_cpp(mean, sd, point, true, false);
    expect_true(draw_ta < 0.0);
    double lower = -1.0;
    double upper = 1.0;
    double draw = rttnorm_cpp(mean, sd, lower, upper);
    expect_true(draw > -1.0);
    expect_true(draw < 1.0);
  }

}
