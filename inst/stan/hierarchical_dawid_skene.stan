data {
  int<lower=1> N;               // total number of annotations
  int<lower=1> J;               // number of annotators
  int<lower=1> K;               // number of annotation categories
  int<lower=1> I;               // number of items
  int<lower=1, upper=I> ii[N];  // item index for annotation n
  int<lower=1, upper=J> jj[N];  // annotator for annotation n
  int<lower=0, upper=K> y[N];   // annotation for observation n
  vector<lower=0>[K] alpha;     // prior for pi
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
    // non centered parameterization
    beta[j] = zeta + omega .* beta_raw[j];
    // fix last category to 0 (softmax non-identifiability)
    beta_norm[j] = append_col(beta[j], zeros);
    for(k in 1:K) {
      // log_softmax
      beta_norm[j,k] = beta_norm[j,k] - log_sum_exp(beta_norm[j,k]);
    }
  }

  // compute outside the loop for efficency
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
  to_vector(zeta) ~ normal(0,1);
  to_vector(omega) ~ normal(0,1);

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
