#' Extract Item Difficulty Estimates (beta)
#'
#' Returns a tidy table of estimated item difficulties with credible
#' intervals and convergence diagnostics.
#'
#' @param object A fitted model from rasch_fit().
#' @param prob Width of the credible interval (0 to 1). Default 0.95
#'   gives a 95% credible interval.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{item}{Question name.}
#'     \item{mean}{Posterior mean — the "best guess" point estimate.}
#'     \item{median}{Posterior median — robust alternative to the mean.}
#'     \item{sd}{Posterior SD — how uncertain we are about this estimate.}
#'     \item{q_lower, q_upper}{Credible interval bounds.}
#'     \item{rhat}{Convergence diagnostic. Must be < 1.01.}
#'     \item{ess_bulk}{Effective sample size (center). Should be > 400.}
#'     \item{ess_tail}{Effective sample size (tails). Should be > 400.}
#'   }
#'
#' @export
item_params <- function(object, prob = 0.95) {
  assert_birt_fit(object)
  checkmate::assert_number(prob, lower = 0, upper = 1)

  # Calculate quantile cutoffs
  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

  # Extract posterior draws for beta
  draws <- posterior::as_draws_matrix(object$fit$draws("beta"))

  # ---- Compute summaries manually (avoids column naming issues) ----
  # apply(draws, 2, fun) applies 'fun' to each column (each item)
  b_mean <- apply(draws, 2, mean)
  b_median <- apply(draws, 2, stats::median)
  b_sd <- apply(draws, 2, stats::sd)
  b_lower <- apply(draws, 2, stats::quantile, probs = probs[1])
  b_upper <- apply(draws, 2, stats::quantile, probs = probs[2])

  # Convergence diagnostics (these need the posterior package)
  b_rhat <- apply(draws, 2, posterior::rhat)
  b_ess_bulk <- apply(draws, 2, posterior::ess_bulk)
  b_ess_tail <- apply(draws, 2, posterior::ess_tail)

  # Build a clean data frame
  data.frame(
    item = object$item_names,
    mean = b_mean,
    median = b_median,
    sd = b_sd,
    q_lower = b_lower,
    q_upper = b_upper,
    rhat = b_rhat,
    ess_bulk = b_ess_bulk,
    ess_tail = b_ess_tail,
    row.names = NULL, # no row names
    stringsAsFactors = FALSE
  )
}


#' Extract Student Ability Estimates (alpha + delta)
#'
#' Returns each student's TOTAL estimated ability (alpha + delta),
#' not just the deviation.
#'
#' @param object A fitted model from rasch_fit().
#' @param prob Width of the credible interval. Default 0.95.
#'
#' @return A data frame with one row per student.
#'
#' @export
person_params <- function(object, prob = 0.95) {
  assert_birt_fit(object)
  checkmate::assert_number(prob, lower = 0, upper = 1)

  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

  # Get draws for alpha and delta
  alpha_draws <- posterior::as_draws_matrix(object$fit$draws("alpha"))
  delta_draws <- posterior::as_draws_matrix(object$fit$draws("delta"))

  # Total ability = alpha + delta (for each draw)
  theta_draws <- alpha_draws + as.vector(delta_draws)

  # Compute summaries manually
  t_mean <- apply(theta_draws, 2, mean)
  t_median <- apply(theta_draws, 2, stats::median)
  t_sd <- apply(theta_draws, 2, stats::sd)
  t_lower <- apply(theta_draws, 2, stats::quantile, probs = probs[1])
  t_upper <- apply(theta_draws, 2, stats::quantile, probs = probs[2])
  t_rhat <- apply(theta_draws, 2, posterior::rhat)
  t_ess_bulk <- apply(theta_draws, 2, posterior::ess_bulk)
  t_ess_tail <- apply(theta_draws, 2, posterior::ess_tail)

  data.frame(
    person = object$person_ids,
    mean = t_mean,
    median = t_median,
    sd = t_sd,
    q_lower = t_lower,
    q_upper = t_upper,
    rhat = t_rhat,
    ess_bulk = t_ess_bulk,
    ess_tail = t_ess_tail,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Extract the Mean Ability Estimate (delta)
#'
#' Returns the posterior summary for delta, the overall mean ability.
#'
#' @param object A fitted model from rasch_fit().
#' @param prob Width of the credible interval. Default 0.95.
#'
#' @return A one-row data frame with mean, median, sd, CI, rhat, and ESS.
#'
#' @export
delta_param <- function(object, prob = 0.95) {
  assert_birt_fit(object)
  checkmate::assert_number(prob, lower = 0, upper = 1)

  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

  draws <- posterior::as_draws_matrix(object$fit$draws("delta"))

  # delta is a single parameter, so draws is a 1-column matrix
  # We use drop = TRUE to get a plain vector
  d <- as.vector(draws)

  data.frame(
    mean = mean(d),
    median = stats::median(d),
    sd = stats::sd(d),
    q_lower = stats::quantile(d, probs = probs[1], names = FALSE),
    q_upper = stats::quantile(d, probs = probs[2], names = FALSE),
    rhat = posterior::rhat(draws),
    ess_bulk = posterior::ess_bulk(draws),
    ess_tail = posterior::ess_tail(draws),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Extract Discrimination Estimates (a)
#'
#' Returns item discrimination estimates from a 2PL or 3PL fit.
#'
#' @param object A fitted 2PL or 3PL model.
#' @param prob Credible interval width. Default 0.95.
#'
#' @return A data frame with one row per item.
#'
#' @export
discrim_params <- function(object, prob = 0.95) {

  if (!inherits(object, c("birt_2pl_fit", "birt_3pl_fit"))) {
    cli::cli_abort("Discrimination is only available for 2PL and 3PL models.")
  }
  checkmate::assert_number(prob, lower = 0, upper = 1)

  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

  draws <- posterior::as_draws_matrix(object$fit$draws("a"))

  data.frame(
    item     = object$item_names,
    mean     = apply(draws, 2, mean),
    median   = apply(draws, 2, stats::median),
    sd       = apply(draws, 2, stats::sd),
    q_lower  = apply(draws, 2, stats::quantile, probs = probs[1]),
    q_upper  = apply(draws, 2, stats::quantile, probs = probs[2]),
    rhat     = apply(draws, 2, posterior::rhat),
    ess_bulk = apply(draws, 2, posterior::ess_bulk),
    ess_tail = apply(draws, 2, posterior::ess_tail),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Extract Guessing Parameter Estimates (c)
#'
#' Returns guessing parameter estimates from a 3PL fit.
#'
#' @param object A fitted 3PL model.
#' @param prob Credible interval width. Default 0.95.
#'
#' @return A data frame with one row per item.
#'
#' @export
guessing_params <- function(object, prob = 0.95) {

  if (!inherits(object, "birt_3pl_fit")) {
    cli::cli_abort("Guessing parameters are only available for 3PL models.")
  }
  checkmate::assert_number(prob, lower = 0, upper = 1)

  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)

  draws <- posterior::as_draws_matrix(object$fit$draws("c"))

  data.frame(
    item     = object$item_names,
    mean     = apply(draws, 2, mean),
    median   = apply(draws, 2, stats::median),
    sd       = apply(draws, 2, stats::sd),
    q_lower  = apply(draws, 2, stats::quantile, probs = probs[1]),
    q_upper  = apply(draws, 2, stats::quantile, probs = probs[2]),
    rhat     = apply(draws, 2, posterior::rhat),
    ess_bulk = apply(draws, 2, posterior::ess_bulk),
    ess_tail = apply(draws, 2, posterior::ess_tail),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
