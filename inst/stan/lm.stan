// Save this file as src/stan_files/lm.stan
data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
}
model {
  // ... priors, etc.

  y ~ normal(intercept + beta * x, sigma);
}
