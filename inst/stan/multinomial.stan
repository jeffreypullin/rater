data {
  int<lower=2> K;               // number of classes
  int<lower=1> N;               // number of annotations
  int<lower=1> I;               // number of items
  int<lower=1,upper=I> ii[N];   // the item the n-th annotation belongs to
  int y[N];                     // the class of the n-th annotation
  vector<lower=0>[K] alpha;     // prior for pi
  vector<lower=0>[K] beta[K];   // prior for theta
}

parameters {
  simplex[K] pi;
  simplex[K] theta[K];
}

transformed parameters {
  vector[K] log_p_z[I];
  vector[K] log_pi;
  vector[K] log_theta[K];

  log_pi    = log(pi);
  log_theta = log(theta);

  for (i in 1:I) {
    log_p_z[i] = log_pi;
  }

  for (n in 1:N) {
    for (k in 1:K) {
      log_p_z[ii[n], k] = log_p_z[ii[n], k] + log_theta[k, y[n]];
    }
  }

}

model {

  for (k in 1:K) {
    theta[k] ~ dirichlet(beta[k]);
  }

  pi ~ dirichlet(alpha);

  for (i in 1:I) {
     target += log_sum_exp(log_p_z[i]);
  }

}

