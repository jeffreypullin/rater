data {
  int<lower=1> N;                  // number of different amounts
  int<lower=1> K;                  // number of annotation categories
  int<lower=1> J;                  // number of annotators
  real tally[N];                   // total of each different combination
  int<lower=1,upper=K> key[N, J];  //
  vector<lower=0>[K] alpha;        // prior for pi
  vector<lower=0>[K] beta[J, K];   // prior for theta
}

parameters {
  simplex[K] pi;
  simplex[K] theta[J, K];
}

transformed parameters {
  vector[K] log_p_z[N];
  // use the prior for the prevalence
  for (i in 1:N) {
    log_p_z[i] = log(pi);
    for (j in 1:J)
      for (k in 1:K)
         log_p_z[i, k] = log_p_z[i, k] + log(theta[j, k, key[i, j]]);
  }
}

model {
  // prior on pi
  pi ~ dirichlet(alpha);

  for (j in 1:J) {
    for (k in 1:K) {
      theta[j, k] ~ dirichlet(beta[j, k]);
    }
  }

  // adding repeatedly on log scale - multiplication
  for (i in 1:N) {
    target += tally[i] * log_sum_exp(log_p_z[i]);
  }
}
