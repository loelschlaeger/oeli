test_that("finding closest year works", {
  expect_equal(
    find_closest_year(as.Date("2022-06-01")),
    2022
  )
  expect_equal(
    find_closest_year(as.Date("2022-06-30")),
    2022
  )
  expect_equal(
    find_closest_year(as.Date("2022-07-01")),
    2023
  )
  expect_equal(
    find_closest_year(as.Date("2022-12-31")),
    2023
  )
})
