# Fit a 3PL IRT Model

Fits a three-parameter logistic IRT model using CmdStan. The model is:
`P(correct) = c_k + (1 - c_k) * logistic(a_k * (alpha_j + delta - beta_k))`

## Usage

``` r
threepl_fit(
  data,
  prior_delta = c(0, 1),
  prior_alpha_sd = 1.5,
  prior_beta = c(0, 1.5),
  prior_beta_mean = NULL,
  prior_beta_sd = NULL,
  prior_a = c(0, 0.5),
  prior_a_meanlog = NULL,
  prior_a_sdlog = NULL,
  prior_c = c(2, 8),
  prior_c_alpha = NULL,
  prior_c_beta = NULL,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A matrix or data frame of binary (0/1) responses. Rows = students,
  Columns = questions. NA is allowed.

- prior_delta:

  Prior for mean ability: `c(mean, sd)`. Default `c(0, 1)`.

- prior_alpha_sd:

  Prior SD for student ability deviations. Default 1.5.

- prior_beta:

  Prior for item difficulties when all items share the same prior:
  `c(mean, sd)`. Default `c(0, 1.5)`. Ignored if `prior_beta_mean` or
  `prior_beta_sd` is provided.

- prior_beta_mean:

  Numeric vector of length K. Per-item prior means for difficulty.
  Overrides `prior_beta`.

- prior_beta_sd:

  Numeric vector of length K. Per-item prior SDs for difficulty.
  Overrides `prior_beta`.

- prior_a:

  Prior for discrimination when all items share the same prior
  (lognormal): `c(meanlog, sdlog)`. Default `c(0, 0.5)`. Ignored if
  `prior_a_meanlog` or `prior_a_sdlog` is provided.

- prior_a_meanlog:

  Numeric vector of length K. Per-item prior meanlog for discrimination.
  Overrides `prior_a`.

- prior_a_sdlog:

  Numeric vector of length K. Per-item prior sdlog for discrimination.
  Overrides `prior_a`.

- prior_c:

  Prior for guessing when all items share the same prior (beta
  distribution): `c(alpha, beta)`. Default `c(2, 8)`. Ignored if
  `prior_c_alpha` or `prior_c_beta` is provided.

- prior_c_alpha:

  Numeric vector of length K. Per-item alpha parameter for the Beta
  guessing prior. Overrides `prior_c`.

- prior_c_beta:

  Numeric vector of length K. Per-item beta parameter for the Beta
  guessing prior. Overrides `prior_c`.

- chains:

  Number of MCMC chains. Default 4.

- parallel_chains:

  Chains to run in parallel. Default 4.

- iter_warmup:

  Warmup iterations per chain. Default 1000.

- iter_sampling:

  Sampling iterations per chain. Default 1000.

- seed:

  Random seed for reproducibility.

- ...:

  Extra arguments passed to cmdstanr's sample() method.

## Value

An object of class `birt_3pl_fit`.

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- rasch_simulate(J = 500, K = 10, seed = 42)

# Default priors
fit3 <- threepl_fit(sim$data, seed = 123)

# Per-item: items 1-5 are 4-option MC, items 6-10 are 5-option MC
K <- 10
c_alpha <- c(rep(5, 5), rep(5, 5))
c_beta  <- c(rep(15, 5), rep(20, 5))  # mean 0.25 vs 0.20
fit3 <- threepl_fit(sim$data,
  prior_c_alpha = c_alpha,
  prior_c_beta = c_beta,
  seed = 123
)
} # }
```
