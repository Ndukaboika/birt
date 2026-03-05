#' Fit a Bayesian Rasch (1PL) IRT Model
#'
#' Fits a Rasch model using CmdStan. The model is:
#'   `logit(P(correct)) = alpha_j + delta - beta_k`
#'
#' @param data A matrix or data frame of binary (0/1) responses.
#'   Rows = students, Columns = questions. NA is allowed.
#' @param prior_delta Prior for mean ability: `c(mean, sd)`.
#'   Default `c(0.75, 1)` means Normal(0.75, 1).
#' @param prior_alpha_sd Prior SD for student ability deviations.
#'   Default 1 means alpha ~ Normal(0, 1).
#' @param prior_beta_sd Prior SD for item difficulties.
#'   Default 1 means beta ~ Normal(0, 1).
#' @param chains Number of MCMC chains. Default 4.
#' @param parallel_chains Chains to run in parallel. Default 4.
#' @param iter_warmup Warmup iterations per chain. Default 1000.
#' @param iter_sampling Sampling iterations per chain. Default 1000.
#' @param seed Random seed for reproducibility.
#' @param ... Extra arguments passed to cmdstanr's sample() method
#'   (e.g., `adapt_delta = 0.95`).
#'
#' @return An object of class `birt_fit`.
#'
#' @examples
#' \dontrun{
#' sim <- rasch_simulate(J = 200, K = 10, seed = 42)
#'
#' # Default priors
#' fit <- rasch_fit(sim$data, seed = 123)
#'
#' # Custom priors
#' fit <- rasch_fit(sim$data,
#'   prior_delta = c(0, 2),
#'   prior_alpha_sd = 2,
#'   prior_beta_sd = 2,
#'   seed = 123
#' )
#' }
#'
#' @export
rasch_fit <- function(data,
                      prior_delta = c(0, 1),
                      prior_alpha_sd = 1.5,
                      prior_beta_sd = 1.5,
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
  checkmate::assert_number(prior_beta_sd, lower = 0)

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
    prior_beta_sd    = prior_beta_sd
  )

  # --- Compile ---
  stan_file <- system.file("stan", "rasch.stan", package = "birt")
  if (stan_file == "") {
    cli::cli_abort("Stan model file not found. Is {.pkg birt} installed correctly?")
  }

  cli::cli_inform("Compiling Rasch model...")
  mod <- cmdstanr::cmdstan_model(stan_file)

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
        delta    = prior_delta,
        alpha_sd = prior_alpha_sd,
        beta_sd  = prior_beta_sd
      )
    ),
    class = "birt_fit"
  )
}
