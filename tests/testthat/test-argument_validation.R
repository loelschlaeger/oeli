test_that("Argument matching works", {
  expect_equal(
    match_arg("A", "A"),
    "A"
  )
  expect_equal(
    match_arg("A", LETTERS),
    "A"
  )
  expect_error(
    match_arg(c("A", "B"), LETTERS),
    "must be of length 1"
  )
  expect_equal(
    match_arg("lo", c("loooong", "else")),
    "loooong"
  )
  expect_equal(
    match_arg(c("A", "B"), LETTERS, several.ok = TRUE),
    c("A", "B")
  )
  expect_error(
    match_arg(character(), LETTERS),
    "greater or equal 1"
  )
  expect_equal(
    match_arg(character(), LETTERS, none.ok = TRUE),
    character()
  )
  expect_error(
    match_arg("bad", LETTERS),
    "must be one of"
  )
})
