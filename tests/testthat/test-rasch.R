test_that("rasch_simulate returns correct structure", {
  sim <- rasch_simulate(J = 50, K = 5, seed = 42)

  expect_type(sim, "list")
  expect_named(sim, c("data", "delta", "alpha", "beta"))
  expect_equal(nrow(sim$data), 50)
  expect_equal(ncol(sim$data), 5)
  expect_true(all(sim$data %in% c(0, 1)))
  expect_equal(length(sim$alpha), 50)
  expect_equal(length(sim$beta), 5)
})

test_that("rasch_simulate is reproducible with seed", {
  sim1 <- rasch_simulate(J = 20, K = 5, seed = 99)
  sim2 <- rasch_simulate(J = 20, K = 5, seed = 99)

  expect_identical(sim1$data, sim2$data)
  expect_identical(sim1$alpha, sim2$alpha)
})

test_that("rasch_fit rejects bad input", {
  expect_error(rasch_fit("not a matrix"))
  expect_error(rasch_fit(matrix(c(0, 1, 2, 3), nrow = 2)))
  expect_error(rasch_fit(matrix(c(0, 1), nrow = 1)))
})

test_that("input validation works for all models", {
  bad_data <- matrix(c(0, 1, 5, 1), nrow = 2)
  expect_error(rasch_fit(bad_data))
  expect_error(twopl_fit(bad_data))
  expect_error(threepl_fit(bad_data))

  too_small <- matrix(c(0, 1), nrow = 1)
  expect_error(rasch_fit(too_small))
})

test_that("algebra dataset loads correctly", {
  data(algebra)
  expect_true(is.matrix(algebra))
  expect_equal(ncol(algebra), 12)
  expect_true(all(algebra[!is.na(algebra)] %in% c(0, 1)))
  expect_true(nrow(algebra) > 1000)
})
