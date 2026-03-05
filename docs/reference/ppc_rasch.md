# Posterior Predictive Check for Rasch Model

Simulates new datasets from the posterior and compares the mean score to
the observed mean score. If the model fits well, the observed mean (red
dashed line) should fall within the simulated distribution (blue bars).

## Usage

``` r
ppc_rasch(object, nsamples = 100)
```

## Arguments

- object:

  A birt_fit object from rasch_fit().

- nsamples:

  Number of posterior predictive draws (default = 100).

## Value

Invisibly returns a list with:

- y_rep:

  List of simulated response matrices.

- plot:

  The ggplot2 plot object.
