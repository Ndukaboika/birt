#' Item Fit Statistics (Outfit and Infit)
#'
#' Outfit = mean of standardized squared residuals (sensitive to outliers).
#' Infit  = information-weighted mean-square (sensitive to systematic misfit).
#'
#' Both should be near 1.0:
#'   Values greater than 1.3 means more noise than the model expects
#'   Values less than 0.7 means less noise than expected (items too predictable)
#'
#' @param object A bayesRasch_fit object.
#'
#' @return A data frame with columns: item, obs_prop, exp_prop, outfit, infit.
#'
#' @export
item_fit <- function(object) {

  checkmate::assert_class(object, "bayesRasch_fit")

  y  <- object$stan_data$y
  jj <- object$stan_data$jj
  kk <- object$stan_data$kk
  K  <- object$K

  # Get posterior mean estimates
  alpha_draws <- posterior::as_draws_matrix(object$fit$draws("alpha"))
  beta_draws  <- posterior::as_draws_matrix(object$fit$draws("beta"))
  delta_draws <- posterior::as_draws_matrix(object$fit$draws("delta"))

  alpha_mean <- apply(alpha_draws, 2, mean)
  beta_mean  <- apply(beta_draws, 2, mean)
  delta_mean <- mean(delta_draws)

  # Expected probability for each observation
  # Using YOUR model: logit(P) = alpha[j] + delta - beta[k]
  p_exp <- stats::plogis(alpha_mean[jj] + delta_mean - beta_mean[kk])

  # Residuals
  residual        <- y - p_exp                # raw residual
  variance        <- p_exp * (1 - p_exp)      # expected variance under Rasch
  std_residual_sq <- residual^2 / variance    # standardized squared residual

  # Compute per item
  result <- data.frame(
    item     = object$item_names,
    obs_prop = NA_real_,
    exp_prop = NA_real_,
    outfit   = NA_real_,
    infit    = NA_real_,
    stringsAsFactors = FALSE
  )

  for (k in seq_len(K)) {
    idx <- which(kk == k)                           # observations for this item
    result$obs_prop[k] <- mean(y[idx])              # actual proportion correct
    result$exp_prop[k] <- mean(p_exp[idx])           # model-predicted proportion
    result$outfit[k]   <- mean(std_residual_sq[idx]) # outfit mean-square
    result$infit[k]    <- sum(residual[idx]^2) / sum(variance[idx])  # infit
  }

  result
}


#' Person Fit Statistics
#'
#' Same as item_fit() but per student. Identifies students with
#' unusual response patterns (guessing, careless responding, etc.).
#'
#' @param object A bayesRasch_fit object.
#'
#' @return A data frame with columns: person, total_score, outfit, infit.
#'
#' @export
person_fit <- function(object) {

  checkmate::assert_class(object, "bayesRasch_fit")

  y  <- object$stan_data$y
  jj <- object$stan_data$jj
  kk <- object$stan_data$kk
  J  <- object$J

  alpha_draws <- posterior::as_draws_matrix(object$fit$draws("alpha"))
  beta_draws  <- posterior::as_draws_matrix(object$fit$draws("beta"))
  delta_draws <- posterior::as_draws_matrix(object$fit$draws("delta"))

  alpha_mean <- apply(alpha_draws, 2, mean)
  beta_mean  <- apply(beta_draws, 2, mean)
  delta_mean <- mean(delta_draws)

  p_exp <- stats::plogis(alpha_mean[jj] + delta_mean - beta_mean[kk])
  residual        <- y - p_exp
  variance        <- p_exp * (1 - p_exp)
  std_residual_sq <- residual^2 / variance

  result <- data.frame(
    person      = object$person_ids,
    total_score = NA_integer_,
    outfit      = NA_real_,
    infit       = NA_real_,
    stringsAsFactors = FALSE
  )

  for (j in seq_len(J)) {
    idx <- which(jj == j)
    result$total_score[j] <- sum(y[idx])
    result$outfit[j]      <- mean(std_residual_sq[idx])
    result$infit[j]       <- sum(residual[idx]^2) / sum(variance[idx])
  }

  result
}
