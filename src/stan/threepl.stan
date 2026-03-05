// Three-Parameter Logistic (3PL) IRT model with user-configurable priors
// P(correct) = c[k] + (1 - c[k]) * logistic(a[k] * (alpha[j] + delta - beta[k]))

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
  real prior_a_meanlog;
  real<lower=0> prior_a_sdlog;
  real<lower=0> prior_c_alpha;
  real<lower=0> prior_c_beta;
}

parameters {
  real delta;
  vector[J] alpha;
  vector[K] beta;
  vector<lower=0>[K] a;
  vector<lower=0, upper=1>[K] c;
}

model {
  alpha ~ normal(0, prior_alpha_sd);
  beta ~ normal(0, prior_beta_sd);
  delta ~ normal(prior_delta_mean, prior_delta_sd);
  a ~ lognormal(prior_a_meanlog, prior_a_sdlog);
  c ~ beta(prior_c_alpha, prior_c_beta);

  {
    vector[N] prob;
    prob = c[kk] + (1 - c[kk]) .* inv_logit(a[kk] .* (alpha[jj] + delta - beta[kk]));
    y ~ bernoulli(prob);
  }
}
