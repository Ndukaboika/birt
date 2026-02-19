
// Rasch IRT model
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
  int<lower=1> J;                          // number of students
  int<lower=1> K;                          // number of questions
  int<lower=1> N;                          // number of observations
  array[N] int<lower=1, upper=J> jj;       // student for observation n
  array[N] int<lower=1, upper=K> kk;       // question for observation n
  array[N] int<lower=0, upper=1> y;        // correctness for observation n
}

parameters {
  real delta;                     // mean student ability
  vector[J] alpha;                // ability of student j - mean ability
  vector[K] beta;                 // difficulty of question k
  vector<lower=0>[K] a;           // item discriminations (must be positive)
  vector<lower=0, upper=1>[K] c;
}

model {
  // Priors
  alpha ~ std_normal();
  beta ~ std_normal();
  delta ~ normal(0.75, 1);
  a ~ lognormal(0, 0.5);
  c ~ beta(5, 23)

  // Likelihood (vectorized)
 // Fully vectorized likelihood — no loops
  vector[N] prob = c[kk] + (1 - c[kk]) .* inv_logit(a[kk] .* (alpha[jj] + delta - beta[kk]));
  y ~ bernoulli(prob);
}
