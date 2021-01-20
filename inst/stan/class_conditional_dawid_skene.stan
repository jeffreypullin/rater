data {
  int<lower=1> N;               // total number of annotations
  int<lower=1> J;               // number of annotators
  int<lower=1> K;               // number of annotation categories
  int<lower=1> I;               // number of items
  int<lower=1, upper=I> ii[N];  // item index for annotation n
  int<lower=1, upper=J> jj[N];  // annotator for annotation n
  int<lower=0, upper=K> y[N];   // annotation for observation n
  vector<lower=0>[K] alpha;     // prior for pi
  // Assume the same across raters
  vector<lower=0>[K] beta_1;    // prior for theta
  vector<lower=0>[K] beta_2;
}

parameters {
  simplex[K] pi;
  matrix<lower=0, upper=1>[J, K] theta;
}

transformed parameters {
  vector[K] log_p_z[I];

  for (i in 1:I) {
    log_p_z[i] = log(pi);
  }

  for (n in 1:N) {
    for (k in 1:K) {
      if (k == y[n]) {
        log_p_z[ii[n], k] = log_p_z[ii[n], k] + log(theta[jj[n], k]);
      } else {
        log_p_z[ii[n], k] = log_p_z[ii[n], k] + log1m(theta[jj[n], k]) - log(K - 1);
      }
    }
  }

}

model {
  // prior on pi
  pi ~ dirichlet(alpha);

  for (j in 1:J) {
    for (k in 1:K) {
      theta[j, k] ~ beta(beta_1[k], beta_2[k]);
    }
  }

  for (i in 1:I) {
    // log_sum_exp used for numerical stability
    target += log_sum_exp(log_p_z[i]);
  }
}

generated quantities {
  vector[I] log_lik;
  for (i in 1:I) {
    log_lik[i] = log_sum_exp(log_p_z[i]);
  }
}
