/* Bayesian implementation of Dawid and Skene's noisy categorical rating model.
 * This implementation requires data in a 'long' format in order to allow
 * incomplete designs. This implementation is heavily based on the implementation
 * of this model for complete designs given in the Stan Manual section TODO.
 * as well as publicly avaliable code for fitting MAP estimates via EM
 * for the same model. That code can be veiwed here:
 * This code was written before the author was aware of an implemtation of this
 * model in Paun et. al. 2018.
 */

data {
  int<lower=1> N;               // total number of annotations
  int<lower=1> J;               // number of annotators
  int<lower=1> K;               // number of annotation categories
  int<lower=1> I;               // number of items
  int<lower=1, upper=I> ii[N];  // item index for annotation n
  int<lower=1, upper=J> jj[N];  // annotator for annotation n
  int<lower=0, upper=K> y[N];   // annotation for observation n
  vector<lower=0>[K] alpha;     // prior for pi
  vector<lower=0>[K] beta[J, K];   // prior for theta
}

parameters {
  simplex[K] pi;
  simplex[K] theta[J, K];
}

transformed parameters {
  vector[K] log_p_z[I];
  for (i in 1:I) {
    log_p_z[i] = log(pi);
  }
  for (n in 1:N) {
    for (k in 1:K) {
      // Here we marginalise over the latent discrete paramter
      log_p_z[ii[n], k] = log_p_z[ii[n], k] + log(theta[jj[n], k, y[n]]);
    }
  }
}

model {
  // prior on pi
  pi ~ dirichlet(alpha);

  for (j in 1:J) {
    for (k in 1:K) {
       //prior on theta
       theta[j, k] ~ dirichlet(beta[j, k]);
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
