# Fit a 2PL IRT Model

Fits a two-parameter logistic IRT model using CmdStan. The model is:
`logit(P(correct)) = a_k * (alpha_j + delta - beta_k)`

## Usage

``` r
twopl_fit(
  data,
  prior_delta = c(0.75, 1),
  prior_alpha_sd = 1,
  prior_beta_sd = 1,
  prior_a = c(0, 0.5),
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
  `c(0, 0.5)` means LogNormal(0, 0.5), centered around 1.0.

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

# Custom priors
fit2 <- twopl_fit(sim$data,
  prior_a = c(0, 1),
  seed = 123
)
} # }
```
