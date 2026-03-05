# Plot Item Characteristic Curves (ICCs)

An ICC shows the probability of getting an item correct as a function of
ability. Each item gets one curve. Harder items have curves shifted to
the right.

## Usage

``` r
plot_icc(object, items = NULL, theta_range = c(-4, 4), ci = TRUE, ...)
```

## Arguments

- object:

  A birt_fit object.

- items:

  Which items to plot (integer vector). Default: all.

- theta_range:

  Range of ability values to show. Default c(-4, 4).

- ci:

  Show 95% credible bands? Default TRUE.

- ...:

  Ignored.

## Value

A ggplot2 object.
