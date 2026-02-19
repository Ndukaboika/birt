// Three-Parameter Logistic (3PL) IRT Model
// P(correct) = c[k] + (1 - c[k]) * logistic(a[k] * (alpha[j] + delta - beta[k]))

data {
  int<lower=1> J;
  int<lower=1> K;
  int<lower=1> N;
  array[N] int<lower=1, upper=J> jj;
  array[N] int<lower=1, upper=K> kk;
  array[N] int<lower=0, upper=1> y;
}

parameters {
  real delta;
  vector[J] alpha;
  vector[K] beta;
  vector<lower=0>[K] a;
  vector<lower=0, upper=1>[K] c;
}

model {
  alpha ~ std_normal();
  beta ~ std_normal();
  delta ~ normal(0.75, 1);
  a ~ lognormal(0, 0.5);
  c ~ beta(5, 23);

  // Likelihood
  {
    vector[N] prob;
    prob = c[kk] + (1 - c[kk]) .* inv_logit(a[kk] .* (alpha[jj] + delta - beta[kk]));
    y ~ bernoulli(prob);
  }
}
