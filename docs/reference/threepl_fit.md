# Fit a 3PL IRT Model

Fits a three-parameter logistic IRT model using CmdStan. The model is:
`P(correct) = c_k + (1 - c_k) * logistic(a_k * (alpha_j + delta - beta_k))`

## Usage

``` r
threepl_fit(
  data,
  prior_delta = c(0.75, 1),
  prior_alpha_sd = 1,
  prior_beta_sd = 1,
  prior_a = c(0, 0.5),
  prior_c = c(5, 23),
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

  Prior for mean ability: `c(mean, sd)`. Default `c(0.75, 1)`.

- prior_alpha_sd:

  Prior SD for student ability deviations. Default 1.

- prior_beta_sd:

  Prior SD for item difficulties. Default 1.

- prior_a:

  Prior for discrimination (lognormal): `c(meanlog, sdlog)`. Default
  `c(0, 0.5)`.

- prior_c:

  Prior for guessing (beta distribution): `c(alpha, beta)`. Default
  `c(5, 23)` means Beta(5, 23) with mean 0.18. For 4-option MC items,
  try `c(5, 15)` (mean 0.25). For 5-option MC items, try `c(5, 20)`
  (mean 0.20). For free-response items, try `c(1, 19)` (mean 0.05).

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

# 4-option multiple choice
fit3 <- threepl_fit(sim$data,
  prior_c = c(5, 15),
  seed = 123
)
} # }
```
