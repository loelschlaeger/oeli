test_that("checks of format 'YYYY-MM-DD' for dates work", {
  expect_equal(
    check_date(date = "2000-01-01"),
    as.Date("2000-01-01")
  )
  expect_error(
    check_date(date = "2000-02-30"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
  expect_error(
    check_date(date = "2000-13-01"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
  expect_error(
    check_date(date = "01.01.2021"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
})
