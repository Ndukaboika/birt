# Extract the Mean Ability Estimate (delta)

Returns the posterior summary for delta, the overall mean ability.

## Usage

``` r
delta_param(object, prob = 0.95)
```

## Arguments

- object:

  A fitted model from rasch_fit().

- prob:

  Width of the credible interval. Default 0.95.

## Value

A one-row data frame with mean, median, sd, CI, rhat, and ESS.
