test_that("simulation of Markov chain works", {
  expect_length(
    simulate_markov_chain(Gamma = diag(2), T = 10, delta = c(0.5, 0.5)),
    10
  )
})
