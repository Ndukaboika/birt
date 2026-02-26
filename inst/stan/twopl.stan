
// Two-Parameter Logistic (2PL) IRT model with user-configurable priors
// logit(P(correct)) = a[k] * (alpha[j] + delta - beta[k])
//
// This model estimates:
//   delta    = overall mean ability of the student population
//   alpha[j] = how much student j deviates from the mean ability
//   beta[k]  = difficulty of question k
//
// So a student\'s TOTAL ability = alpha[j] + delta
//
// The probability of student j answering question k correctly:
//   logit(P(y = 1)) = alpha[j] - beta[k] + delta



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
}

parameters {
  real delta;
  vector[J] alpha;
  vector[K] beta;
  vector<lower=0>[K] a;
}

model {
  alpha ~ normal(0, prior_alpha_sd);
  beta ~ normal(0, prior_beta_sd);
  delta ~ normal(prior_delta_mean, prior_delta_sd);
  a ~ lognormal(prior_a_meanlog, prior_a_sdlog);

  y ~ bernoulli_logit(a[kk] .* (alpha[jj] + delta - beta[kk]));
}
