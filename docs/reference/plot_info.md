# Test Information Function

Shows where on the ability scale the test is most precise. In Rasch,
item info = P(1-P), which peaks at ability = difficulty. Test info = sum
of all item infos.

## Usage

``` r
plot_info(
  object,
  items = NULL,
  theta_range = c(-4, 4),
  show_items = FALSE,
  ...
)
```

## Arguments

- object:

  A birt_fit object.

- items:

  Which items to include. Default: all.

- theta_range:

  Range of ability values. Default c(-4, 4).

- show_items:

  Show individual item curves? Default FALSE.

- ...:

  Ignored.

## Value

A ggplot2 object.
