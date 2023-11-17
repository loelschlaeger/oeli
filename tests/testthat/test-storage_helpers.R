test_that("basic Index example works", {
  my_index <- Index$new()
  checkmate::expect_r6(my_index, "Index")
  my_index$
    add(42, c("number", "rational"))$
    add(pi, c("number", "!rational"))$
    add("fear of black cats", c("text", "!rational"))$
    add("wearing a seatbelt", c("text", "rational"))$
    add(mean, "function")
  expect_equal(
    my_index$get("rational"),
    list(42, "wearing a seatbelt")
  )
  expect_length(
    my_index$get("!rational"),
    2
  )
  expect_equal(
    my_index$get(c("text", "!rational")),
    list("fear of black cats")
  )
  expect_length(
    my_index$get(ids = 4:5, id_names = TRUE),
    2
  )
  expect_equal(
    my_index$get(c("text", "!text")),
    list()
  )
  expect_equal(
    my_index$get(c("text", "!text"), logical = "or"),
    my_index$get("all")
  )
  expect_equal(
    my_index$remove(ids = c(2, 4:5), shift_ids = TRUE)$get("all"),
    list(42, "fear of black cats")
  )
})

test_that("lists can be merged", {
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2)),
    list(a = 1, b = 2)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list()),
    list(a = 1, b = 2)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4)),
    list(a = 1, b = 2, c = 4)
  )
  expect_equal(
    merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4), list("d" = 5)),
    list(a = 1, b = 2, c = 4, d = 5)
  )
})
