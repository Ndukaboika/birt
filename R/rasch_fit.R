#' Fit a Bayesian Rasch Model
#'
#' This is the main function of the package. Give it a matrix of 0/1
#' responses and it will estimate student abilities and item difficulties
#' using Bayesian MCMC sampling via CmdStan.
#'
#' The model is:
#'  logit(P(correct)) = alpha\[j\] + delta - beta\[k\]
#'
#' Where delta is the overall mean ability, alpha\[j\] is student j's
#' deviation from the mean, and beta\[k\] is the difficulty of question k.
#'
#'
#' @param data A matrix or data frame of binary (0/1) responses.
#'   Rows = students, Columns = questions. NA is allowed (missing data).
#' @param chains Number of MCMC chains. Default 4. More chains = better
#'   convergence diagnostics but takes longer.
#' @param parallel_chains How many chains to run at the same time. Default 4.
#' @param iter_warmup Warmup iterations per chain (discarded). Default 1000.
#'   The sampler uses these to find the right region of parameter space.
#' @param iter_sampling Sampling iterations per chain (kept). Default 1000.
#'   Total posterior draws = chains * iter_sampling (default: 4000).
#' @param seed Random seed for reproducibility.
#' @param ... Extra arguments passed to cmdstanr's sample() method.
#'
#' @return An object of class "birt_fit" containing:
#'   \describe{
#'     \item{fit}{The CmdStanMCMC fit object (for advanced use).}
#'     \item{data}{The original response matrix.}
#'     \item{stan_data}{The data list that was passed to Stan.}
#'     \item{item_names}{Character vector of question names.}
#'     \item{person_ids}{Character vector of student IDs.}
#'     \item{J}{Number of students.}
#'     \item{K}{Number of questions.}
#'   }
#'
#' @examples
#' \dontrun{
#' sim <- rasch_simulate(J = 200, K = 10, seed = 42)
#' fit <- rasch_fit(sim$data, seed = 123)
#' summary(fit)
#' }
#'
#' @export
rasch_fit <- function(data,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 1000,
                      iter_sampling = 1000,
                      seed = NULL,
                      ...) {
  # ==============================================================
  # PART 1: Validate the input data
  # ==============================================================

  # Check that 'data' is either a matrix or a data frame
  checkmate::assert(
    checkmate::check_matrix(data), # is it a matrix?
    checkmate::check_data_frame(data), # or a data frame?
    combine = "or" # either one is OK
  )

  # Convert to matrix if it's a data frame
  # (Stan needs numeric matrices, not data frames)
  data <- as.matrix(data)

  # Check that every non-missing value is either 0 or 1
  # !is.na(data) gives TRUE for non-missing cells
  # data[!is.na(data)] extracts just those values
  # %in% c(0L, 1L) checks each value is 0 or 1
  if (!all(data[!is.na(data)] %in% c(0L, 1L))) {
    cli::cli_abort("All non-missing values in {.arg data} must be 0 or 1.")
  }

  # Get dimensions
  J <- nrow(data) # number of students (rows)
  K <- ncol(data) # number of questions (columns)

  # Need at least 2 students and 2 questions
  if (J < 2) cli::cli_abort("Need at least 2 students (rows).")
  if (K < 2) cli::cli_abort("Need at least 2 questions (columns).")

  # ==============================================================
  # PART 2: Get names for students and questions
  # ==============================================================

  # Use existing names if the matrix has them, otherwise make generic ones
  item_names <- colnames(data)
  if (is.null(item_names)) item_names <- paste0("Q", seq_len(K))

  person_ids <- rownames(data)
  if (is.null(person_ids)) person_ids <- paste0("S", seq_len(J))

  # ==============================================================
  # PART 3: Convert matrix to long format for Stan
  # ==============================================================
  # Your Stan model expects data in "long format":
  #   jj = which student   (e.g., 1, 1, 1, ..., 2, 2, 2, ...)
  #   kk = which question  (e.g., 1, 2, 3, ..., 1, 2, 3, ...)
  #   y  = the response    (e.g., 1, 0, 1, ..., 0, 1, 1, ...)
  #
  # which(arr.ind = TRUE) returns the row/column positions
  # of all non-NA cells. This handles missing data automatically!

  obs <- which(!is.na(data), arr.ind = TRUE)

  # Build the data list that matches YOUR Stan model's data{} block
  stan_data <- list(
    J  = J, # number of students
    K  = K, # number of questions
    N  = nrow(obs), # total observations (J*K minus any NAs)
    jj = as.integer(obs[, 1]), # student index per observation
    kk = as.integer(obs[, 2]), # question index per observation
    y  = as.integer(data[obs]) # the 0/1 response
  )

  # ==============================================================
  # PART 4: Compile the Stan model
  # ==============================================================

  # system.file() finds where the .stan file was installed
  # When someone installs the package, inst/stan/birt.stan
  # becomes accessible at this path
  stan_file <- system.file("stan", "birt.stan", package = "birt")

  # If it's not found, the package wasn't installed properly
  if (stan_file == "") {
    cli::cli_abort(
      "Stan model file not found. Is {.pkg birt} installed correctly?"
    )
  }

  # Compile the Stan code to C++ (slow first time, cached after)
  cli::cli_inform("Compiling Stan model...")
  mod <- cmdstanr::cmdstan_model(stan_file)

  # ==============================================================
  # PART 5: Run MCMC sampling
  # ==============================================================

  cli::cli_inform(
    "Sampling ({chains} chains x {iter_sampling} iterations)..."
  )

  fit <- mod$sample(
    data            = stan_data, # our prepared data
    chains          = chains, # number of chains
    parallel_chains = parallel_chains, # how many in parallel
    iter_warmup     = iter_warmup, # warmup (discarded)
    iter_sampling   = iter_sampling, # kept samples
    seed            = seed, # reproducibility
    ... # any extra options
  )

  # ==============================================================
  # PART 6: Bundle everything into a return object
  # ==============================================================

  # structure() creates a list and assigns it a class name
  # The class "birt_fit" lets us define custom methods:
  #   print.birt_fit, summary.birt_fit, plot.birt_fit
  out <- structure(
    list(
      fit        = fit, # the raw CmdStanMCMC object
      data       = data, # original response matrix
      stan_data  = stan_data, # what we sent to Stan
      item_names = item_names, # question names
      person_ids = person_ids, # student IDs
      J          = J, # number of students
      K          = K # number of questions
    ),
    class = "birt_fit" # our custom class name
  )

  out
}
