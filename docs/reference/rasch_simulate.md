# Simulate Data from a Rasch Model

Creates fake response data where you KNOW the true parameters. This is
useful for testing that the model works (parameter recovery), learning
the package, and running simulation studies.

## Usage

``` r
rasch_simulate(
  J = 200,
  K = 10,
  delta_true = 0.75,
  alpha_sd = 1,
  beta = NULL,
  seed = NULL
)
```

## Arguments

- J:

  Number of students. Default 200.

- K:

  Number of questions. Default 10.

- delta_true:

  True mean ability. Default 0.75 (matches the prior).

- alpha_sd:

  Standard deviation of student ability deviations. Default 1.

- beta:

  Values for item difficulties. If NULL, creates K evenly spaced values
  from -2 (easiest) to 2 (hardest).

- seed:

  Random seed for reproducibility. Same seed = same data every time.

## Value

A list with:

- data:

  A J x K matrix of 0/1 responses.

- delta:

  The true mean ability used.

- alpha:

  Vector of true student deviations (length J).

- beta:

  Vector of true item difficulties (length K).

## Examples

``` r
sim <- rasch_simulate(J = 100, K = 5, seed = 42)
head(sim$data) # first few rows of the response matrix
#>    Q1 Q2 Q3 Q4 Q5
#> S1  1  1  1  1  1
#> S2  1  1  1  0  0
#> S3  1  1  1  1  1
#> S4  1  1  1  0  0
#> S5  1  1  0  1  0
#> S6  1  0  1  1  0
sim$beta # true item difficulties
#> [1] -2 -1  0  1  2
sim$delta # true mean ability
#> [1] 0.75
```
