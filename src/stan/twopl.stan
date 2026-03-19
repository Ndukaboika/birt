// Two-Parameter Logistic (2PL) IRT model with per-item configurable priors
// logit(P(correct)) = a[k] * (alpha[j] + delta - beta[k])

data {
  int<lower=1> J;
  int<lower=1> K;
  int<lower=1> N;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=1, upper=K> kk;
  array[N] int<lower=0, upper=1> y;

  // Prior hyperparameters
  real prior_delta_mean;
  real<lower=0> prior_delta_sd;
  real<lower=0> prior_alpha_sd;

  // Per-item priors for difficulty
  vector[K] prior_beta_mean;
  vector<lower=0>[K] prior_beta_sd;

  // Per-item priors for discrimination
  vector[K] prior_a_meanlog;
  vector<lower=0>[K] prior_a_sdlog;
}

parameters {
  real delta;
  vector[J] alpha;
  vector[K] beta;
  vector<lower=0>[K] a;
}

model {
  alpha ~ normal(0, prior_alpha_sd);
  delta ~ normal(prior_delta_mean, prior_delta_sd);

  // Per-item priors
  beta ~ normal(prior_beta_mean, prior_beta_sd);
  a ~ lognormal(prior_a_meanlog, prior_a_sdlog);

  y ~ bernoulli_logit(a[kk] .* (alpha[jj] + delta - beta[kk]));
}
