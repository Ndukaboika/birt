# Plot a Rasch Model Fit

Plot a Rasch Model Fit

## Usage

``` r
# S3 method for class 'birt_fit'
plot(x, type = c("icc", "wright", "info", "trace"), items = NULL, ...)
```

## Arguments

- x:

  A birt_fit object.

- type:

  Which plot to make:

  "icc"

  :   Item Characteristic Curves — P(correct) vs ability.

  "wright"

  :   Wright Map — persons & items on same scale.

  "info"

  :   Test Information Function — where the test measures best.

  "trace"

  :   MCMC trace plots — check if chains mixed well.

- items:

  Which items to plot (integer indices). Default: all.

- ...:

  Extra arguments passed to the plotting function.
