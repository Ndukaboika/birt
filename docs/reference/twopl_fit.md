# Fit a 2PL IRT Model

Fits a two-parameter logistic IRT model using CmdStan. The model is:
`logit(P(correct)) = a_k * (alpha_j + delta - beta_k)`

## Usage

``` r
twopl_fit(
  data,
  prior_delta = c(0, 1),
  prior_alpha_sd = 1.5,
  prior_beta = c(0, 1.5),
  prior_beta_mean = NULL,
  prior_beta_sd = NULL,
  prior_a = c(0, 0.5),
  prior_a_meanlog = NULL,
  prior_a_sdlog = NULL,
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

An object of class `birt_2pl_fit`.

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- rasch_simulate(J = 300, K = 10, seed = 42)

# Default priors
fit2 <- twopl_fit(sim$data, seed = 123)

# Per-item: item 5 is known to be poorly discriminating
K <- ncol(sim$data)
a_meanlog <- rep(0, K)
a_sdlog <- rep(0.5, K)
a_meanlog[5] <- -0.5   # prior centered below 1.0
a_sdlog[5] <- 0.3      # tighter prior
fit2 <- twopl_fit(sim$data,
  prior_a_meanlog = a_meanlog,
  prior_a_sdlog = a_sdlog,
  seed = 123
)
} # }
```
