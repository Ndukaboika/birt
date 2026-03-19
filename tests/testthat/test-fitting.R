# These tests actually run Stan — skip if CmdStan is not installed
skip_if_not_installed("instantiate")
skip_if_not(
  tryCatch({instantiate::stan_package_model(name = "rasch", package = "birt"); TRUE}, error = function(e) FALSE),
  message = "Stan models not compiled"
)

test_that("rasch_fit runs on simulated data", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- rasch_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)

  expect_s3_class(fit, "birt_fit")
  expect_equal(fit$J, 100)
  expect_equal(fit$K, 5)
  expect_equal(fit$model, "Rasch")

  # Check priors are stored
  expect_equal(fit$priors$delta, c(0, 1))
  expect_equal(fit$priors$alpha_sd, 1.5)
  expect_equal(fit$priors$beta_mean, rep(0, 5))
  expect_equal(fit$priors$beta_sd, rep(1.5, 5))
})

test_that("rasch_fit works with custom priors", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- rasch_fit(sim$data,
                   prior_delta = c(0, 2),
                   prior_alpha_sd = 2,
                   prior_beta = c(0, 2),
                   chains = 2, iter_sampling = 200, seed = 123
  )

  expect_equal(fit$priors$delta, c(0, 2))
  expect_equal(fit$priors$alpha_sd, 2)
  expect_equal(fit$priors$beta_sd, rep(2, 5))
})

test_that("rasch_fit works with per-item priors", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  b_mean <- c(0, 0, 2, 0, -1)
  b_sd <- c(1.5, 1.5, 0.5, 1.5, 0.5)

  fit <- rasch_fit(sim$data,
                   prior_beta_mean = b_mean,
                   prior_beta_sd = b_sd,
                   chains = 2, iter_sampling = 200, seed = 123
  )

  expect_equal(fit$priors$beta_mean, b_mean)
  expect_equal(fit$priors$beta_sd, b_sd)
})

test_that("parameter extraction works for Rasch", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- rasch_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)

  items <- item_params(fit)
  expect_equal(nrow(items), 5)
  expect_true(all(c("item", "mean", "sd", "q_lower", "q_upper", "rhat") %in% names(items)))

  persons <- person_params(fit)
  expect_equal(nrow(persons), 100)

  d <- delta_param(fit)
  expect_equal(nrow(d), 1)
})

test_that("diagnostics work for Rasch", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- rasch_fit(sim$data, chains = 2, iter_sampling = 200, seed = 456)

  ifit <- item_fit(fit)
  expect_equal(nrow(ifit), 5)
  expect_true(all(c("outfit", "infit") %in% names(ifit)))

  pfit <- person_fit(fit)
  expect_equal(nrow(pfit), 100)
})

test_that("twopl_fit runs and returns correct class", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- twopl_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)

  expect_s3_class(fit, "birt_2pl_fit")
  expect_equal(fit$model, "2PL")
  expect_equal(fit$priors$a_meanlog, rep(0, 5))
  expect_equal(fit$priors$a_sdlog, rep(0.5, 5))

  disc <- discrim_params(fit)
  expect_equal(nrow(disc), 5)
  expect_true(all(disc$mean > 0))
})

test_that("threepl_fit runs and returns correct class", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- suppressWarnings(
    threepl_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)
  )

  expect_s3_class(fit, "birt_3pl_fit")
  expect_equal(fit$model, "3PL")
  expect_equal(fit$priors$c_alpha, rep(2, 5))
  expect_equal(fit$priors$c_beta, rep(8, 5))

  guess <- guessing_params(fit)
  expect_equal(nrow(guess), 5)
  expect_true(all(guess$mean >= 0 & guess$mean <= 1))
})

test_that("rasch_fit works on algebra dataset", {
  data(algebra)
  fit <- rasch_fit(algebra, chains = 2, iter_sampling = 200, seed = 123)

  expect_s3_class(fit, "birt_fit")
  expect_equal(fit$K, 12)

  items <- item_params(fit)
  expect_equal(nrow(items), 12)
  expect_true(all(items$rhat < 1.05))

  ifit <- item_fit(fit)
  expect_equal(nrow(ifit), 12)
})

test_that("parameter recovery works on simulated data", {
  sim <- rasch_simulate(J = 300, K = 10, seed = 42)
  fit <- rasch_fit(sim$data, chains = 2, iter_sampling = 500, seed = 123)

  items <- item_params(fit)
  recovery_cor <- cor(sim$beta, items$mean)
  expect_gt(recovery_cor, 0.90)

  d <- delta_param(fit)
  expect_true(abs(d$mean - sim$delta) < 1.0)
})

test_that("plots run without error", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- rasch_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)

  expect_s3_class(plot(fit, type = "icc"), "ggplot")
  expect_s3_class(plot(fit, type = "wright"), "ggplot")
  expect_s3_class(plot(fit, type = "info"), "ggplot")
})

test_that("2PL plots run without error", {
  sim <- rasch_simulate(J = 100, K = 5, seed = 42)
  fit <- twopl_fit(sim$data, chains = 2, iter_sampling = 200, seed = 123)

  expect_s3_class(plot(fit, type = "icc"), "ggplot")
  expect_s3_class(plot(fit, type = "info"), "ggplot")
})
