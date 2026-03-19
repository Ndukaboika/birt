# birt

> Bayesian Item Response Theory models in R, powered by Stan.

**birt** (Bayesian IRT) fits Item Response Theory models using full
Bayesian estimation via
[CmdStan](https://mc-stan.org/users/interfaces/cmdstan). It gives you
posterior distributions, credible intervals, and convergence diagnostics
for every parameter — not just point estimates. Stan models are
pre-compiled during package installation using the
[instantiate](https://wlandau.github.io/instantiate/) package, so there
is no compilation delay at runtime.

## Features

- **Three models**: Rasch (1PL), 2PL, and 3PL for dichotomous response
  data
- **Full Bayesian inference**: Credible intervals and posterior
  distributions via Hamiltonian Monte Carlo
- **User-configurable priors**: Weakly informative defaults that work
  out of the box, with class-level and per-item control for users who
  want informative priors
- **Built-in diagnostics**: Outfit/infit statistics, convergence checks
  (Rhat, ESS), and posterior predictive checks
- **Publication-ready plots**: Item characteristic curves (with credible
  bands), Wright maps, and test information functions
- **Missing data**: Handled automatically — no imputation needed
- **Real data included**: Algebra test dataset (1,382 students, 12
  items) for practice and demonstration

## Supported Models

| Model | Function | Item Parameters | Minimum N |
|----|----|----|----|
| Rasch (1PL) | [`rasch_fit()`](https://ndukaboika.github.io/birt/reference/rasch_fit.md) | difficulty | ~100 |
| 2PL | [`twopl_fit()`](https://ndukaboika.github.io/birt/reference/twopl_fit.md) | difficulty + discrimination | ~200 |
| 3PL | [`threepl_fit()`](https://ndukaboika.github.io/birt/reference/threepl_fit.md) | difficulty + discrimination + guessing | ~500 |

## Installation

``` r
# 1. Install cmdstanr
install.packages("cmdstanr", repos = c(
  "https://stan-dev.r-universe.dev/",
  getOption("repos")
))

# 2. Install CmdStan (~5 minutes, one-time setup)
cmdstanr::install_cmdstan()

# 3. Install birt (from source to trigger Stan compilation)
pak::pak("Ndukaboika/birt")
```

## Quick Example

``` r
library(birt)

# Simulate test data: 300 students, 10 items
sim <- rasch_simulate(J = 300, K = 10, seed = 42)

# Fit a Rasch model
fit <- rasch_fit(sim$data, seed = 123)

# View results
summary(fit)

# Extract parameters
item_params(fit)       # item difficulties with credible intervals
person_params(fit)     # student abilities
delta_param(fit)       # overall mean ability

# Diagnostics
item_fit(fit)          # outfit and infit statistics

# Plots
plot(fit, type = "icc")     # Item Characteristic Curves
plot(fit, type = "wright")  # Wright Map
plot(fit, type = "info")    # Test Information Function
```

### Fitting a 2PL Model

``` r
fit2 <- twopl_fit(sim$data, seed = 123)
summary(fit2)
discrim_params(fit2)     # item discrimination estimates
plot(fit2, type = "icc") # ICCs with varying slopes
```

### Fitting a 3PL Model

``` r
sim_large <- rasch_simulate(J = 500, K = 10, seed = 42)
fit3 <- threepl_fit(sim_large$data, seed = 123)
guessing_params(fit3)    # guessing parameter estimates
```

## Custom Priors

Default priors are weakly informative and work for most tests. Override
them when you have domain knowledge.

### Class-Level Priors

Set the same prior for all items of a parameter type:

``` r
# Wider difficulty prior for all items
fit <- rasch_fit(data,
  prior_delta    = c(0, 2),       # Normal(0, 2) for mean ability
  prior_alpha_sd = 2,             # Normal(0, 2) for ability deviations
  prior_beta     = c(0, 2),       # Normal(0, 2) for all item difficulties
  seed = 123
)

# 4-option MC guessing for all items
fit3 <- threepl_fit(data,
  prior_c = c(5, 15),             # Beta(5, 15), mean = 0.25
  seed = 123
)
```

### Per-Item Priors

Set a different prior for each individual item:

``` r
K <- ncol(data)

# Item 3 is known to be hard, item 7 is known to be easy
b_mean <- rep(0, K)
b_mean[3] <- 2.0
b_mean[7] <- -1.5

b_sd <- rep(1.5, K)
b_sd[c(3, 7)] <- 0.5    # tighter prior for items we know about

fit <- rasch_fit(data,
  prior_beta_mean = b_mean,
  prior_beta_sd = b_sd,
  seed = 123
)

# Mixed item formats: items 1-5 are 4-option MC, items 6-10 are 5-option MC
c_alpha <- rep(5, 10)
c_beta  <- c(rep(15, 5), rep(20, 5))   # mean 0.25 vs 0.20

fit3 <- threepl_fit(data,
  prior_c_alpha = c_alpha,
  prior_c_beta = c_beta,
  seed = 123
)

# Check what priors were used
fit$priors
```

### Default Prior Summary

| Parameter | Default | Class-level | Per-item |
|----|----|----|----|
| Mean ability (δ) | Normal(0, 1) | `prior_delta = c(0, 1)` | — |
| Ability deviation (α) | Normal(0, 1.5) | `prior_alpha_sd = 1.5` | — |
| Item difficulty (β) | Normal(0, 1.5) | `prior_beta = c(0, 1.5)` | `prior_beta_mean`, `prior_beta_sd` |
| Discrimination (a) | LogNormal(0, 0.5) | `prior_a = c(0, 0.5)` | `prior_a_meanlog`, `prior_a_sdlog` |
| Guessing (c) | Beta(2, 8) | `prior_c = c(2, 8)` | `prior_c_alpha`, `prior_c_beta` |

## Real Data

``` r
data(algebra)
dim(algebra)  # 1382 students, 12 items

fit_alg <- rasch_fit(algebra, seed = 123)
summary(fit_alg)
plot(fit_alg, type = "wright")
```

## Learn More

- **[Getting
  Started](https://ndukaboika.github.io/birt/articles/getting-started.html)**
  — Full tutorial covering all three models, priors, diagnostics, and
  plots
- **[Function Reference](https://ndukaboika.github.io/birt/reference/)**
  — Documentation for every function
- **[Prior Distributions
  Guide](https://ndukaboika.github.io/birt/articles/getting-started.html#prior-distributions)**
  — Default priors, customization, and sensitivity analysis

## Citation

If you use birt in your research, please cite:

    Boika, N. (2026). birt: Bayesian Item Response Theory models in R.
    R package. https://github.com/Ndukaboika/birt

## References

- Luo, Y., & Jiao, H. (2018). Using the Stan program for Bayesian item
  response theory. *Educational and Psychological Measurement, 78*(3),
  384–408.
- Stan Development Team (2024). *Stan User’s Guide*, §1.11:
  Item-Response Theory Models.
- Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A weakly
  informative default prior distribution for logistic and other
  regression models. *Annals of Applied Statistics, 2*(4), 1360–1383.
