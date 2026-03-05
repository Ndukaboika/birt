#' Fit a 3PL IRT Model
#'
#' Fits a three-parameter logistic IRT model using CmdStan.
#' The model is:
#'   `P(correct) = c_k + (1 - c_k) * logistic(a_k * (alpha_j + delta - beta_k))`
#'
#' @param data A matrix or data frame of binary (0/1) responses.
#'   Rows = students, Columns = questions. NA is allowed.
#' @param prior_delta Prior for mean ability: `c(mean, sd)`.
#'   Default `c(0.75, 1)`.
#' @param prior_alpha_sd Prior SD for student ability deviations. Default 1.
#' @param prior_beta_sd Prior SD for item difficulties. Default 1.
#' @param prior_a Prior for discrimination (lognormal): `c(meanlog, sdlog)`.
#'   Default `c(0, 0.5)`.
#' @param prior_c Prior for guessing (beta distribution): `c(alpha, beta)`.
#'   Default `c(5, 23)` means Beta(5, 23) with mean 0.18.
#'   For 4-option MC items, try `c(5, 15)` (mean 0.25).
#'   For 5-option MC items, try `c(5, 20)` (mean 0.20).
#'   For free-response items, try `c(1, 19)` (mean 0.05).
#' @param chains Number of MCMC chains. Default 4.
#' @param parallel_chains Chains to run in parallel. Default 4.
#' @param iter_warmup Warmup iterations per chain. Default 1000.
#' @param iter_sampling Sampling iterations per chain. Default 1000.
#' @param seed Random seed for reproducibility.
#' @param ... Extra arguments passed to cmdstanr's sample() method.
#'
#' @return An object of class `birt_3pl_fit`.
#'
#' @examples
#' \dontrun{
#' sim <- rasch_simulate(J = 500, K = 10, seed = 42)
#'
#' # Default priors
#' fit3 <- threepl_fit(sim$data, seed = 123)
#'
#' # 4-option multiple choice
#' fit3 <- threepl_fit(sim$data,
#'   prior_c = c(5, 15),
#'   seed = 123
#' )
#' }
#'
#' @export
threepl_fit <- function(data,
                        prior_delta = c(0, 1),
                        prior_alpha_sd = 1.5,
                        prior_beta_sd = 1.5,
                        prior_a = c(0, 0.5),
                        prior_c = c(2, 8),
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

  if (J < 500) {
    cli::cli_warn(
      "The 3PL model works best with 500+ students. You have {J}.
       Consider using {.fn rasch_fit} or {.fn twopl_fit} for smaller samples."
    )
  }

  # --- Validate priors ---
  checkmate::assert_numeric(prior_delta, len = 2)
  checkmate::assert_number(prior_alpha_sd, lower = 0)
  checkmate::assert_number(prior_beta_sd, lower = 0)
  checkmate::assert_numeric(prior_a, len = 2)
  checkmate::assert_numeric(prior_c, len = 2, lower = 0)

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
    prior_beta_sd    = prior_beta_sd,
    prior_a_meanlog  = prior_a[1],
    prior_a_sdlog    = prior_a[2],
    prior_c_alpha    = prior_c[1],
    prior_c_beta     = prior_c[2]
  )

  # --- Compile ---
  mod <- instantiate::stan_package_model(
    name = "threepl",
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
      model      = "3PL",
      priors     = list(
        delta    = prior_delta,
        alpha_sd = prior_alpha_sd,
        beta_sd  = prior_beta_sd,
        a        = prior_a,
        c_param  = prior_c
      )
    ),
    class = "birt_3pl_fit"
  )
}
