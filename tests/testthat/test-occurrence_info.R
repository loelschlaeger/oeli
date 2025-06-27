test_that("multiplication works", {
  x <- data.frame("A" = c(1, 1, 2), "B" = LETTERS[1:3], c(NA, NA, NA))
  colnames(x)[3] <- NA
  expect_equal(
    occurrence_info(x, relative = FALSE, named = FALSE),
    c("2x 1, 1x 2", "1x A, 1x B, 1x C", "3x NA")
  )
  expect_equal(
    occurrence_info(x, relative = TRUE, named = FALSE),
    c("67% 1, 34% 2", "34% A, 34% B, 34% C", "100% NA")
  )
  expect_equal(
    occurrence_info(x, relative = FALSE, named = TRUE),
    c("A: 2x 1, 1x 2", "B: 1x A, 1x B, 1x C", "3x NA")
  )
  expect_equal(
    occurrence_info(x, relative = TRUE, named = TRUE),
    c("A: 67% 1, 34% 2", "B: 34% A, 34% B, 34% C", "100% NA")
  )
  expect_equal(
    occurrence_info(data.frame()),
    character()
  )
})
