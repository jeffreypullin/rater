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

transformed data {
  vector[K] zeros = rep_vector(0,K);
}

parameters {
  simplex[K] pi;
  matrix[K, K-1] beta_raw[J];
  matrix[K, K-1] zeta;
  matrix<lower=0>[K,K-1] omega;
}

transformed parameters {
  matrix[K,K-1] beta[J];
  matrix[K,K] beta_norm[J];
  vector[K] log_p_z[I];
  vector[K] log_pi;

  for(j in 1:J) {
    // Non centered parameterization.
    beta[j] = zeta + omega .* beta_raw[j];
    // Fix last category to 0 (softmax non-identifiability).
    beta_norm[j] = append_col(beta[j], zeros);
    for(k in 1:K) {
      // Log softmax
      beta_norm[j,k] = beta_norm[j,k] - log_sum_exp(beta_norm[j,k]);
    }
  }

  // Compute outside the loop for efficency.
  log_pi = log(pi);
  for (i in 1:I) {
    log_p_z[i] = log_pi;
  }

  for (n in 1:N) {
    for (k in 1:K) {
      log_p_z[ii[n], k] = log_p_z[ii[n], k] + beta_norm[jj[n], k, y[n]];
    }
  }

}

model {

  pi ~ dirichlet(alpha);
  for (k in 1:K) {
    for (i in 1:(K - 1)) {
      if (k == i) {
        zeta[k, i] ~ normal(6, 1);
      } else {
        zeta[k, i] ~ normal(0, 1);
      }
    }
  }
  to_vector(omega) ~ normal(0, 1);

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
