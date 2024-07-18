test_that("transition probabiliy matrix can be sampled", {
  expect_true(
    check_transition_probability_matrix(
      sample_transition_probability_matrix(3)
    )
  )
})
