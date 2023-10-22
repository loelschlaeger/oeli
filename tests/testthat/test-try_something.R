test_that("try an expression silently works", {
  expect_equal(try_silent(1 + 1), 2)
  expect_s3_class(try_silent(1 + "1"), "fail")
})
