
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
  real delta;                 // mean student ability
  array[J] real alpha;        // ability of student j - mean ability
  array[K] real beta;         // difficulty of question k
}

model {
  alpha ~ std_normal();       // prior: alpha ~ Normal(0, 1)
  beta ~ std_normal();        // prior: beta ~ Normal(0, 1)
  delta ~ normal(0.75, 1);    // prior: delta ~ Normal(0.75, 1)

  for (n in 1:N) {
    y[n] ~ bernoulli_logit(alpha[jj[n]] - beta[kk[n]] + delta);
  }
}
