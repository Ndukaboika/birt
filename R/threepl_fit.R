#' Fit a 3PL IRT Model
#'
#' Fits a three-parameter logistic IRT model using CmdStan.
#' Extends the 2PL by adding a guessing parameter (c) representing
#' the probability of a correct response by pure chance.
#'
#' #' The model is:
#'   `P(correct) = c_k + (1 - c_k) * logistic(a_k * (alpha_j + delta - beta_k))`
#'
#' Where:
#'   - delta = overall mean ability
#'   - alpha_j = student j's deviation from mean ability
#'   - beta_k = difficulty of question k
#'   - a_k = discrimination of question k
#'   - c_k = guessing parameter for question k (NEW vs 2PL)
#'
#' @param data A matrix or data frame of binary (0/1) responses.
#'   Rows = students, Columns = questions. NA is allowed.
#' @param chains Number of MCMC chains. Default 4.
#' @param parallel_chains Chains to run in parallel. Default 4.
#' @param iter_warmup Warmup iterations per chain. Default 1000.
#' @param iter_sampling Sampling iterations per chain. Default 1000.
#' @param seed Random seed for reproducibility.
#' @param ... Extra arguments passed to cmdstanr's sample() method.
#'
#' @return An object of class "birt_3pl_fit".
#'
#' @examples
#' \dontrun{
#' sim <- rasch_simulate(J = 500, K = 10, seed = 42)
#' fit <- threepl_fit(sim$data, seed = 123)
#' summary(fit)
#' }
#'
#' @export
threepl_fit <- function(data,
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
    y  = as.integer(data[obs])
  )

  # --- Compile ---
  stan_file <- system.file("stan", "threepl.stan", package = "birt")
  if (stan_file == "") {
    cli::cli_abort("Stan model file not found. Is {.pkg birt} installed correctly?")
  }

  cli::cli_inform("Compiling 3PL model...")
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
      model      = "3PL"
    ),
    class = "birt_3pl_fit"
  )
}
