# Fit a Bayesian Rasch (1PL) IRT Model

Fits a Rasch model using CmdStan. The model is:
`logit(P(correct)) = alpha_j + delta - beta_k`

## Usage

``` r
rasch_fit(
  data,
  prior_delta = c(0, 1),
  prior_alpha_sd = 1.5,
  prior_beta = c(0, 1.5),
  prior_beta_mean = NULL,
  prior_beta_sd = NULL,
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

  Prior for mean ability: `c(mean, sd)`. Default `c(0, 1)` means
  Normal(0, 1).

- prior_alpha_sd:

  Prior SD for student ability deviations. Default 1.5 means alpha ~
  Normal(0, 1.5).

- prior_beta:

  Prior for item difficulties when all items share the same prior:
  `c(mean, sd)`. Default `c(0, 1.5)`. Ignored if `prior_beta_mean` or
  `prior_beta_sd` is provided.

- prior_beta_mean:

  Numeric vector of length K (one per item). Per-item prior means for
  difficulty. Overrides `prior_beta`.

- prior_beta_sd:

  Numeric vector of length K (one per item). Per-item prior SDs for
  difficulty. Overrides `prior_beta`.

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

An object of class `birt_fit`.

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- rasch_simulate(J = 200, K = 10, seed = 42)

# Default priors (same for all items)
fit <- rasch_fit(sim$data, seed = 123)

# Per-item priors: item 3 is known to be hard
K <- ncol(sim$data)
b_mean <- rep(0, K)
b_mean[3] <- 2.0
fit <- rasch_fit(sim$data, prior_beta_mean = b_mean, seed = 123)
} # }
```
