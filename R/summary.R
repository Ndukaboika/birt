#' @export
summary.birt <- function(object, ...) {
  draws <- object$fit$draws()

  alpha_vars <- grep("^alpha\\[", posterior::variables(draws), value = TRUE)
  beta_vars <- grep("^beta_raw\\[", posterior::variables(draws), value = TRUE)
  delta_vars <- grep("^delta$", posterior::variables(draws), value = TRUE)

  alpha_summary <- posterior::summarise_draws(draws, variables = alpha_vars)
  beta_summary <- posterior::summarise_draws(draws, variables = beta_vars)
  delta_summary <- posterior::summarise_draws(draws, variables = delta_vars)

  list(
    delta = delta_summary,
    student_ability = alpha_summary,
    item_difficulty = beta_summary
  )
}
