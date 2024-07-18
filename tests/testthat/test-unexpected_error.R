test_that("handling of an unknown error works", {
  expect_error(
    unexpected_error(issue_link = "bad_link"),
    "Assertion on 'issue_link' failed"
  )
  expect_error(
    unexpected_error(
      msg = "upps",
      issue_link = "https://github.com/loelschlaeger/oeli/issues"
    ),
    "upps"
  )
})
