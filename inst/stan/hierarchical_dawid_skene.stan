data {
  int<lower=1> N;               // Total number of ratings.
  int<lower=1> J;               // Number of raters.
  int<lower=1> K;               // Number of rating categories.
  int<lower=1> I;               // Number of items.
  int<lower=1, upper=I> ii[N];  // Item index for rating n.
  int<lower=1, upper=J> jj[N];  // Rater for annotation n.
  int<lower=0, upper=K> y[N];   // Rating for observation n.
  vector<lower=0>[K] alpha;     // Prior on pi.
}

parameters {
  simplex[K] pi;
  matrix[K, K] beta_raw[J];
  matrix[K, K] mu;
  matrix<lower=0>[K, K] sigma;
}

transformed parameters {
  matrix[K,K] beta[J];
  vector[K] log_p_z[I];
  vector[K] log_pi;

  for(j in 1:J) {
    // Non centered parameterization.
    beta[j] = mu + sigma .* beta_raw[j];
    for(k in 1:K) {
      // Log softmax
      beta[j,k] = beta[j,k] - log_sum_exp(beta[j,k]);
    }
  }

  // Compute outside the loop for efficency.
  log_pi = log(pi);
  for (i in 1:I) {
    log_p_z[i] = log_pi;
  }

  for (n in 1:N) {
    for (k in 1:K) {
      log_p_z[ii[n], k] = log_p_z[ii[n], k] + beta[jj[n], k, y[n]];
    }
  }

}

model {

  pi ~ dirichlet(alpha);
  for (k in 1:K) {
    for (i in 1:K) {
      if (k == i) {
        mu[k, i] ~ normal(2, 1);
      } else {
        mu[k, i] ~ normal(0, 1);
      }
    }
  }
  to_vector(sigma) ~ normal(0, 1);

  for(j in 1:J) {
    // part of the non centered parameterization
    to_vector(beta_raw[j]) ~ normal(0, 1);
  }

  for (i in 1:I) {
    target += log_sum_exp(log_p_z[i]);
  }

}

generated quantities {
  vector[I] log_lik;
  for (i in 1:I) {
    log_lik[i] = log_sum_exp(log_p_z[i]);
  }
}
