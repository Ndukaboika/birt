# Extract Item Difficulty Estimates (beta)

Returns a tidy table of estimated item difficulties with credible
intervals and convergence diagnostics.

## Usage

``` r
item_params(object, prob = 0.95)
```

## Arguments

- object:

  A fitted model from rasch_fit().

- prob:

  Width of the credible interval (0 to 1). Default 0.95 gives a 95%
  credible interval.

## Value

A data frame with columns:

- item:

  Question name.

- mean:

  Posterior mean — the "best guess" point estimate.

- median:

  Posterior median — robust alternative to the mean.

- sd:

  Posterior SD — how uncertain we are about this estimate.

- q_lower, q_upper:

  Credible interval bounds.

- rhat:

  Convergence diagnostic. Must be \< 1.01.

- ess_bulk:

  Effective sample size (center). Should be \> 400.

- ess_tail:

  Effective sample size (tails). Should be \> 400.
