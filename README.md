# birt

<!-- badges: start -->
<!-- badges: end -->

**birt** (Bayesian IRT) fits Item Response Theory models using Bayesian
estimation via [CmdStan](https://mc-stan.org/users/interfaces/cmdstan).
Stan models are pre-compiled during package installation using the
[instantiate](https://wlandau.github.io/instantiate/) package, so there
is no compilation delay at runtime.

Supported models:

| Model | Function | Item Parameters |
|-------|----------|-----------------|
| Rasch (1PL) | `rasch_fit()` | difficulty |
| 2PL | `twopl_fit()` | difficulty + discrimination |
| 3PL | `threepl_fit()` | difficulty + discrimination + guessing |

All priors are user-configurable. Sensible defaults are provided based on
the Stan User's Guide.

## Installation

```r
# 1. Install cmdstanr
install.packages("cmdstanr", repos = c(
  "https://stan-dev.r-universe.dev/",
  getOption("repos")
))

# 2. Install CmdStan
cmdstanr::install_cmdstan()

# 3. Install birt (from source to trigger Stan compilation)
pak::pak("Ndukaboika/birt")
```

## Quick Example

```r
library(birt)

# Simulate data
sim <- rasch_simulate(J = 300, K = 10, seed = 42)

# Fit a Rasch model
fit <- rasch_fit(sim$data, seed = 123)
fit
summary(fit)

# Extract parameters
item_params(fit)
delta_param(fit)

# Plots
plot(fit, type = "icc")
plot(fit, type = "wright")
plot(fit, type = "info")

# Diagnostics
item_fit(fit)
```

## Custom Priors

Every prior can be overridden:

```r
fit <- rasch_fit(data,
  prior_delta    = c(0, 2),       # Normal(0, 2) for mean ability
  prior_alpha_sd = 2,             # Normal(0, 2) for ability deviations
  prior_beta_sd  = 2,             # Normal(0, 2) for item difficulties
  seed = 123
)

# 3PL for 4-option multiple choice
fit3 <- threepl_fit(data,
  prior_c = c(5, 15),             # Beta(5, 15), mean = 0.25
  seed = 123
)
```

## Real Data

```r
data(algebra)
fit <- rasch_fit(algebra, seed = 123)
summary(fit)
plot(fit, type = "wright")
```

## Learn More

- [Getting Started vignette](https://ndukaboika.github.io/birt/articles/getting-started.html)
- [Function reference](https://ndukaboika.github.io/birt/reference/)
- [Prior distributions guide](https://ndukaboika.github.io/birt/articles/getting-started.html#prior-distributions)
