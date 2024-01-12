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
