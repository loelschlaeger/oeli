test_that("brute force matching works", {
  expect_equal(
    match_numerics(1:9, 9:1),
    9:1
  )
})
