#' Print a Rasch Model Fit
#'
#' Shows a quick overview when you type the fitted object name.
#'
#' @param x A bayesRasch_fit object.
#' @param ... Ignored.
#'
#' @export
print.bayesRasch_fit <- function(x, ...) {

  # Header
  cli::cli_h1("Bayesian Rasch Model ({.pkg bayesRasch})")
  cli::cli_inform("Students: {x$J} | Questions: {x$K} | Observations: {x$stan_data$N}")

  # Missing data info
  n_total   <- x$J * x$K                   # max possible observations
  n_missing <- n_total - x$stan_data$N      # how many are missing
  if (n_missing > 0) {
    pct <- round(100 * n_missing / n_total, 1)
    cli::cli_inform("Missing responses: {n_missing} ({pct}%)")
  }

  # Quick convergence check
  diag   <- x$fit$diagnostic_summary(quiet = TRUE)
  n_div  <- sum(diag$num_divergent)
  n_tree <- sum(diag$num_max_treedepth)

  if (n_div > 0) {
    cli::cli_warn("{n_div} divergent transition{?s} detected! Results may be unreliable.")
  }
  if (n_tree > 0) {
    cli::cli_warn("{n_tree} transition{?s} hit max treedepth.")
  }
  if (n_div == 0 && n_tree == 0) {
    cli::cli_inform("Diagnostics: No divergences or treedepth warnings.")
  }

  cli::cli_inform("")
  cli::cli_inform("Use {.fn summary} for parameter estimates.")
  cli::cli_inform("Use {.fn plot} for visualizations.")

  invisible(x)
}


#' Summarize a Rasch Model Fit
#'
#' Displays item difficulties, person ability distribution, mean ability
#' (delta), and convergence diagnostics.
#'
#' @param object A bayesRasch_fit object.
#' @param prob Credible interval width. Default 0.95.
#' @param ... Ignored.
#'
#' @export
summary.bayesRasch_fit <- function(object, prob = 0.95, ...) {

  cli::cli_h1("Bayesian Rasch Model Summary")

  # --- Delta (mean ability) ---
  cli::cli_h2("Mean Ability (delta)")
  d <- delta_param(object, prob = prob)
  cat(sprintf(
    "  Estimate: %.3f [%.3f, %.3f]\n",
    d$mean, d$q_lower, d$q_upper
  ))

  # --- Item difficulties ---
  cli::cli_h2("Item Difficulty Estimates (beta)")
  items <- item_params(object, prob = prob)
  print(items, digits = 3, row.names = FALSE)

  # --- Person abilities (summary, not all 200+ rows) ---
  cli::cli_h2("Person Ability Summary (alpha + delta)")
  persons <- person_params(object, prob = prob)
  cat(sprintf(
    "  Mean: %.3f (SD = %.3f)\n  Range: [%.3f, %.3f]\n",
    mean(persons$mean), stats::sd(persons$mean),
    min(persons$mean), max(persons$mean)
  ))

  # --- Convergence diagnostics ---
  cli::cli_h2("Convergence Diagnostics")
  all_rhat <- c(d$rhat, items$rhat, persons$rhat)
  all_ess  <- c(d$ess_bulk, items$ess_bulk, persons$ess_bulk)
  cat(sprintf(
    "  Rhat range:       [%.4f, %.4f]  (want: all < 1.01)\n",
    min(all_rhat, na.rm = TRUE), max(all_rhat, na.rm = TRUE)
  ))
  cat(sprintf(
    "  ESS (bulk) range: [%.0f, %.0f]  (want: all > 400)\n",
    min(all_ess, na.rm = TRUE), max(all_ess, na.rm = TRUE)
  ))

  invisible(list(delta = d, items = items, persons = persons))
}


#' Plot a Rasch Model Fit
#'
#' @param x A bayesRasch_fit object.
#' @param type Which plot to make:
#'   \describe{
#'     \item{"icc"}{Item Characteristic Curves — P(correct) vs ability.}
#'     \item{"wright"}{Wright Map — persons & items on same scale.}
#'     \item{"info"}{Test Information Function — where the test measures best.}
#'     \item{"trace"}{MCMC trace plots — check if chains mixed well.}
#'   }
#' @param items Which items to plot (integer indices). Default: all.
#' @param ... Extra arguments passed to the plotting function.
#'
#' @export
plot.bayesRasch_fit <- function(x, type = c("icc", "wright", "info", "trace"),
                                items = NULL, ...) {

  type <- match.arg(type)

  switch(type,
         icc   = plot_icc(x, items = items, ...),
         wright = plot_wright_map(x, ...),
         info  = plot_info(x, items = items, ...),
         trace = bayesplot::mcmc_trace(
           x$fit$draws(variables = c("delta", "beta"))
         )
  )
}
