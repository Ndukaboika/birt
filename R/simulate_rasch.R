#' Simulate Data from a Rasch Model
#'
#' Creates fake response data where you KNOW the true parameters.
#' This is useful for testing that the model works (parameter recovery),
#' learning the package, and running simulation studies.
#'
#' @param J Number of students. Default 200.
#' @param K Number of questions. Default 10.
#' @param delta_true True mean ability. Default 0.75 (matches the prior).
#' @param alpha_sd Standard deviation of student ability deviations. Default 1.
#' @param beta Values for item difficulties. If NULL, creates K evenly
#'   spaced values from -2 (easiest) to 2 (hardest).
#' @param seed Random seed for reproducibility. Same seed = same data every time.
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{A J x K matrix of 0/1 responses.}
#'     \item{delta}{The true mean ability used.}
#'     \item{alpha}{Vector of true student deviations (length J).}
#'     \item{beta}{Vector of true item difficulties (length K).}
#'   }
#'
#' @examples
#' sim <- rasch_simulate(J = 100, K = 5, seed = 42)
#' head(sim$data)    # first few rows of the response matrix
#' sim$beta          # true item difficulties
#' sim$delta         # true mean ability
#'
#' @export
rasch_simulate <- function(J = 200,
                           K = 10,
                           delta_true = 0.75,
                           alpha_sd = 1,
                           beta = NULL,
                           seed = NULL) {

  # --- Input validation ---
  # These catch mistakes early with clear messages instead of cryptic errors

  # assert_int: must be a whole number
  # lower = 2: must be at least 2 (can't fit a model with 1 student or 1 item)
  checkmate::assert_int(J, lower = 2)
  checkmate::assert_int(K, lower = 2)

  # assert_number: must be a single number (not a vector, not NA)
  checkmate::assert_number(delta_true)
  checkmate::assert_number(alpha_sd, lower = 0)  # SD can't be negative

  # --- Set random seed ---
  # If you provide seed = 42, you'll get the exact same data every time
  # This is crucial for reproducible research
  if (!is.null(seed)) set.seed(seed)

  # --- Generate true student abilities ---
  # alpha[j] = how much student j deviates from the mean
  # Drawn from Normal(0, alpha_sd)
  # Most students will be within +/- 3*alpha_sd of the mean
  alpha <- stats::rnorm(J, mean = 0, sd = alpha_sd)

  # --- Generate true item difficulties ---
  if (is.null(beta)) {
    # Default: evenly spaced from -2 (easy) to +2 (hard)
    # Example with K=5: beta = c(-2, -1, 0, 1, 2)
    beta <- seq(-2, 2, length.out = K)
  } else {
    # If user provided their own, check it's the right length
    checkmate::assert_numeric(beta, len = K)
  }

  # --- Generate response probabilities ---
  # For each student-question pair:
  #   logit(P) = alpha[j] + delta - beta[k]
  #
  # outer(a, b, "-") creates a J x K matrix:
  #   entry [j, k] = a[j] - b[k]
  # Then we add delta to every entry
  logit_p <- outer(alpha + delta_true, beta, "-")

  # plogis() converts log-odds to probabilities
  # plogis(x) = 1 / (1 + exp(-x))
  # So if logit_p = 0, then P = 0.5 (50/50 chance)
  #    if logit_p = 2, then P ≈ 0.88 (likely correct)
  #    if logit_p = -2, then P ≈ 0.12 (likely wrong)
  prob_matrix <- stats::plogis(logit_p)

  # --- Generate binary responses ---
  # rbinom(n, size=1, prob) flips n coins, each with its own probability
  # Returns 0 or 1 for each
  data <- matrix(
    stats::rbinom(J * K, size = 1, prob = prob_matrix),
    nrow = J,
    ncol = K
  )

  # --- Add readable names ---
  colnames(data) <- paste0("Q", seq_len(K))    # Q1, Q2, ..., Q10
  rownames(data) <- paste0("S", seq_len(J))    # S1, S2, ..., S200

  # --- Return everything ---
  # We return the true parameters alongside the data so you can
  # compare estimated vs true (parameter recovery)
  list(
    data = data,
    delta = delta_true,
    alpha = alpha,
    beta = beta
  )
}
