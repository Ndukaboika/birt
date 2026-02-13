test_that("rasch_simulate returns correct structure", {

  sim <- rasch_simulate(J = 50, K = 5, seed = 123)

  # Should return a list with 4 elements

  expect_type(sim, "list")
  expect_named(sim, c("data", "delta", "alpha", "beta"))

  # Data should be a 50 x 5 matrix
  expect_equal(dim(sim$data), c(50, 5))

  # Alpha should have 50 values, beta should have 5
  expect_equal(length(sim$alpha), 50)
  expect_equal(length(sim$beta), 5)

  # All responses should be 0 or 1
  expect_true(all(sim$data %in% c(0L, 1L)))
})

test_that("rasch_simulate validates inputs", {

  # Can't have fewer than 2 students or items
  expect_error(rasch_simulate(J = 1))
  expect_error(rasch_simulate(K = 1))
})

test_that("rasch_fit rejects bad data", {

  # Non-binary data should fail
  bad_data <- matrix(c(0, 1, 2, 1), nrow = 2)
  expect_error(rasch_fit(bad_data), "0 or 1")

  # Single row or column should fail
  expect_error(rasch_fit(matrix(c(0, 1), nrow = 1)), "at least 2")
  expect_error(rasch_fit(matrix(c(0, 1), ncol = 1)), "at least 2")
})





