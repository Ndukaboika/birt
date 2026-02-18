#' Posterior Predictive Check for Rasch Model
#'
#' Simulates new datasets from the posterior and compares the mean score
#' to the observed mean score. If the model fits well, the observed mean
#' (red dashed line) should fall within the simulated distribution (blue bars).
#'
#' @param object A birt_fit object from rasch_fit().
#' @param nsamples Number of posterior predictive draws (default = 100).
#'
#' @return Invisibly returns a list with:
#'   \describe{
#'     \item{y_rep}{List of simulated response matrices.}
#'     \item{plot}{The ggplot2 plot object.}
#'   }
#'
#' @export
ppc_rasch <- function(object, nsamples = 100) {
  # --- Check input ---
  # Updated to match our class name "birt_fit"
  checkmate::assert_class(object, "birt_fit")

  fit <- object$fit

  # --- Extract posterior draws ---
  draws <- fit$draws()
  draws_array <- posterior::as_draws_array(draws)

  J <- object$J # number of students
  K <- object$K # number of questions
  N <- J * K # total cells in response matrix

  # --- Select a subset of draws ---
  # If we have 4000 draws but only want 100, take evenly spaced ones
  n_draws <- dim(draws_array)[1]
  idx <- seq(1, n_draws, length.out = min(nsamples, n_draws))

  # --- Simulate new datasets from each posterior draw ---
  y_rep_list <- vector("list", length(idx))

  for (i in seq_along(idx)) {
    d <- idx[i]

    # Extract parameter values for this single draw
    theta <- draws_array[d, , grep("^alpha\\[", dimnames(draws_array)[[3]])]
    beta <- draws_array[d, , grep("^beta\\[", dimnames(draws_array)[[3]])]
    delta <- draws_array[d, 1, "delta"]

    # Compute probability matrix: P(correct) = logistic(alpha + delta - beta)
    prob_mat <- matrix(0, nrow = J, ncol = K)
    for (j in 1:J) {
      for (k in 1:K) {
        prob_mat[j, k] <- stats::plogis(theta[j] + delta - beta[k])
      }
    }

    # Simulate new 0/1 responses from these probabilities
    y_rep <- matrix(
      stats::rbinom(N, size = 1, prob = as.vector(prob_mat)),
      nrow = J, ncol = K
    )
    y_rep_list[[i]] <- y_rep
  }

  # --- Compare observed vs simulated mean scores ---
  # Observed mean: average of all 0/1 responses in the real data
  mean_y_obs <- mean(object$stan_data$y)

  # Simulated means: one per posterior draw
  mean_y_rep <- sapply(y_rep_list, function(x) mean(x))

  # --- Plot ---
  df <- data.frame(mean_y_rep = mean_y_rep)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mean_y_rep)) +
    ggplot2::geom_histogram(
      binwidth = 0.01, fill = "skyblue", color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = mean_y_obs, color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::labs(
      title = "Posterior Predictive Check (Mean Score)",
      subtitle = "Red line = observed | Blue bars = simulated from model",
      x = "Mean simulated score",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  print(p)

  invisible(list(y_rep = y_rep_list, plot = p))
}
