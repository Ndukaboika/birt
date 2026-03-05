# Extract Student Ability Estimates (alpha + delta)

Returns each student's TOTAL estimated ability (alpha + delta), not just
the deviation.

## Usage

``` r
person_params(object, prob = 0.95)
```

## Arguments

- object:

  A fitted model from rasch_fit().

- prob:

  Width of the credible interval. Default 0.95.

## Value

A data frame with one row per student.
