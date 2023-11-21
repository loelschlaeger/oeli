test_that("handling of an unknown error works", {
  expect_error(
    unknown_error(issue_link = "bad_link"),
    "Assertion on 'issue_link' failed"
  )
  expect_error(
    unknown_error(
      msg = "upps",
      issue_link = "https://github.com/loelschlaeger/oeli/issues"
    ),
    "upps"
  )
})
