// Rasch (1PL) IRT model with user-configurable priors
// logit(P(correct)) = alpha[j] + delta - beta[k]

data {
  int<lower=1> J;
  int<lower=1> K;
  int<lower=1> N;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=1, upper=K> kk;
  array[N] int<lower=0, upper=1> y;

  // Prior hyperparameters (passed from R)
  real prior_delta_mean;
  real<lower=0> prior_delta_sd;
  real<lower=0> prior_alpha_sd;
  real<lower=0> prior_beta_sd;
}

parameters {
  real delta;
  vector[J] alpha;
  vector[K] beta;
}

model {
  alpha ~ normal(0, prior_alpha_sd);
  beta ~ normal(0, prior_beta_sd);
  delta ~ normal(prior_delta_mean, prior_delta_sd);

  y ~ bernoulli_logit(alpha[jj] + delta - beta[kk]);
}
