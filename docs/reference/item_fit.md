# Item Fit Statistics (Outfit and Infit)

Outfit = mean of standardized squared residuals (sensitive to outliers).
Infit = information-weighted mean-square (sensitive to systematic
misfit).

## Usage

``` r
item_fit(object)
```

## Arguments

- object:

  A fitted model from rasch_fit(), twopl_fit(), or threepl_fit().

## Value

A data frame with columns: item, obs_prop, exp_prop, outfit, infit.

## Details

Both should be near 1.0: Values greater than 1.3 means more noise than
the model expects Values less than 0.7 means less noise than expected
(items too predictable)
