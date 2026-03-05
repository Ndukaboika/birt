# Person Fit Statistics

Same as item_fit() but per student. Identifies students with unusual
response patterns (guessing, careless responding, etc.).

## Usage

``` r
person_fit(object)
```

## Arguments

- object:

  A fitted model from rasch_fit(), twopl_fit(), or threepl_fit().

## Value

A data frame with columns: person, total_score, outfit, infit.
