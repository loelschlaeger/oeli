test_that("basic Storage example works", {
  my_Storage <- Storage$new()
  checkmate::expect_r6(my_Storage, "Storage")
  my_Storage$
    add(42, c("number", "rational"))$
    add(pi, c("number", "!rational"))$
    add("fear of black cats", c("text", "!rational"))$
    add("wearing a seatbelt", c("text", "rational"))$
    add(mean, "function")
  expect_equal(
    my_Storage$get("rational"),
    list(42, "wearing a seatbelt")
  )
  expect_length(
    my_Storage$get("!rational"),
    2
  )
  expect_equal(
    my_Storage$get(c("text", "!rational")),
    list("fear of black cats")
  )
  expect_length(
    my_Storage$get(ids = 4:5, id_names = TRUE),
    2
  )
  expect_equal(
    my_Storage$get(c("text", "!text")),
    list()
  )
  expect_equal(
    my_Storage$indices(c("text", "!text"), logical = "or"),
    1:5
  )
  expect_equal(
    my_Storage$indices(c("all", "!rational"), logical = "or"),
    1:5
  )
  expect_equal(
    my_Storage$indices(identifier = c("all", "!rational"), logical = "and"),
    2:3
  )
  expect_equal(
    my_Storage$remove(ids = c(2, 4:5), shift_ids = TRUE)$get("all"),
    list(42, "fear of black cats")
  )
})
