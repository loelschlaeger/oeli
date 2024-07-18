test_that("simulation of Markov chain works", {
  expect_error(
    simulate_markov_chain(Gamma = matrix(1, 2, 2)),
    "Assertion on 'Gamma' failed: Must have row sums equal to 1."
  )
  expect_error(
    simulate_markov_chain(Gamma = diag(2), T = -1),
    "Assertion on 'T' failed: Element 1 is not >= 1."
  )
  expect_error(
    simulate_markov_chain(Gamma = diag(2), T = 10, delta = -1),
    "Assertion on 'delta' failed: Must have length 2, but has length 1."
  )
  expect_length(
    simulate_markov_chain(Gamma = diag(2), T = 10, delta = c(0.5, 0.5)),
    10
  )
})
