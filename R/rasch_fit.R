#' Fit a Bayesian Rasch (1PL) IRT Model
#'
#' Fits a Rasch model using CmdStan. The model is:
#'   `logit(P(correct)) = alpha_j + delta - beta_k`
#'
#' @param data A matrix or data frame of binary (0/1) responses.
#'   Rows = students, Columns = questions. NA is allowed.
#' @param prior_delta Prior for mean ability: `c(mean, sd)`.
#'   Default `c(0, 1)` means Normal(0, 1).
#' @param prior_alpha_sd Prior SD for student ability deviations.
#'   Default 1.5 means alpha ~ Normal(0, 1.5).
#' @param prior_beta Prior for item difficulties when all items share
#'   the same prior: `c(mean, sd)`. Default `c(0, 1.5)`.
#'   Ignored if `prior_beta_mean` or `prior_beta_sd` is provided.
#' @param prior_beta_mean Numeric vector of length K (one per item).
#'   Per-item prior means for difficulty. Overrides `prior_beta`.
#' @param prior_beta_sd Numeric vector of length K (one per item).
#'   Per-item prior SDs for difficulty. Overrides `prior_beta`.
#' @param chains Number of MCMC chains. Default 4.
#' @param parallel_chains Chains to run in parallel. Default 4.
#' @param iter_warmup Warmup iterations per chain. Default 1000.
#' @param iter_sampling Sampling iterations per chain. Default 1000.
#' @param seed Random seed for reproducibility.
#' @param ... Extra arguments passed to cmdstanr's sample() method.
#'
#' @return An object of class `birt_fit`.
#'
#' @examples
#' \dontrun{
#' sim <- rasch_simulate(J = 200, K = 10, seed = 42)
#'
#' # Default priors (same for all items)
#' fit <- rasch_fit(sim$data, seed = 123)
#'
#' # Per-item priors: item 3 is known to be hard
#' K <- ncol(sim$data)
#' b_mean <- rep(0, K)
#' b_mean[3] <- 2.0
#' fit <- rasch_fit(sim$data, prior_beta_mean = b_mean, seed = 123)
#' }
#'
#' @export
rasch_fit <- function(data,
                      prior_delta = c(0, 1),
                      prior_alpha_sd = 1.5,
                      prior_beta = c(0, 1.5),
                      prior_beta_mean = NULL,
                      prior_beta_sd = NULL,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 1000,
                      iter_sampling = 1000,
                      seed = NULL,
                      ...) {

  # --- Validate input ---
  checkmate::assert(
    checkmate::check_matrix(data),
    checkmate::check_data_frame(data),
    combine = "or"
  )

  data <- as.matrix(data)

  if (!all(data[!is.na(data)] %in% c(0L, 1L))) {
    cli::cli_abort("All non-missing values in {.arg data} must be 0 or 1.")
  }

  J <- nrow(data)
  K <- ncol(data)

  if (J < 2) cli::cli_abort("Need at least 2 students (rows).")
  if (K < 2) cli::cli_abort("Need at least 2 questions (columns).")

  # --- Validate priors ---
  checkmate::assert_numeric(prior_delta, len = 2)
  checkmate::assert_number(prior_alpha_sd, lower = 0)
  checkmate::assert_numeric(prior_beta, len = 2)

  # --- Build per-item beta priors ---
  if (!is.null(prior_beta_mean)) {
    checkmate::assert_numeric(prior_beta_mean, len = K)
    b_mean <- prior_beta_mean
  } else {
    b_mean <- rep(prior_beta[1], K)
  }

  if (!is.null(prior_beta_sd)) {
    checkmate::assert_numeric(prior_beta_sd, len = K, lower = 0)
    b_sd <- prior_beta_sd
  } else {
    b_sd <- rep(prior_beta[2], K)
  }

  # --- Names ---
  item_names <- colnames(data)
  if (is.null(item_names)) item_names <- paste0("Q", seq_len(K))

  person_ids <- rownames(data)
  if (is.null(person_ids)) person_ids <- paste0("S", seq_len(J))

  # --- Long format ---
  obs <- which(!is.na(data), arr.ind = TRUE)

  stan_data <- list(
    J  = J,
    K  = K,
    N  = nrow(obs),
    jj = as.integer(obs[, 1]),
    kk = as.integer(obs[, 2]),
    y  = as.integer(data[obs]),
    prior_delta_mean = prior_delta[1],
    prior_delta_sd   = prior_delta[2],
    prior_alpha_sd   = prior_alpha_sd,
    prior_beta_mean  = b_mean,
    prior_beta_sd    = b_sd
  )

  # --- Compile ---
  mod <- instantiate::stan_package_model(
    name = "rasch",
    package = "birt"
  )

  # --- Sample ---
  cli::cli_inform("Sampling ({chains} chains x {iter_sampling} iterations)...")

  fit <- mod$sample(
    data            = stan_data,
    chains          = chains,
    parallel_chains = parallel_chains,
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    seed            = seed,
    ...
  )

  # --- Return ---
  structure(
    list(
      fit        = fit,
      data       = data,
      stan_data  = stan_data,
      item_names = item_names,
      person_ids = person_ids,
      J          = J,
      K          = K,
      model      = "Rasch",
      priors     = list(
        delta     = prior_delta,
        alpha_sd  = prior_alpha_sd,
        beta_mean = b_mean,
        beta_sd   = b_sd
      )
    ),
    class = "birt_fit"
  )
}
