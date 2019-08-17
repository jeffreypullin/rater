context("mcmc")

# This function needs more tests

test_that("mcmc returns correctly", {

  expect_equal(is.rater_fit(ds_fit), TRUE)
  expect_equal(is.mcmc_fit(ds_fit), TRUE)

})

test_that("parse priors are correct", {

  # dawid skene model

  anesthesia_list <- get_stan_data(long_data(anesthesia))

  # test default priors
  K <- anesthesia_list$K
  ds_priors <- parse_priors(dawid_skene(), K)

  # construct the default priors
  default_alpha <- rep(3, K)
  default_beta <- matrix(1, nrow = K, ncol = K)
  diag(default_beta) <- 2.5 * K

  expect_equal(ds_priors$alpha, default_alpha)
  expect_equal(ds_priors$beta, default_beta)

  # test non-default priors
  test_alpha <- rep(9, K)
  test_beta <- matrix(17, nrow = K, ncol = K)
  ds_priors <- parse_priors(dawid_skene(alpha = test_alpha, beta = test_beta), K)

  expect_equal(ds_priors$alpha, test_alpha)
  expect_equal(ds_priors$beta, test_beta)

  # default priors
  hds_priors <- parse_priors(hier_dawid_skene(), K)

  expect_equal(hds_priors$alpha, default_alpha)

  # non-default priors
  hds_priors <- parse_priors(hier_dawid_skene(alpha = test_alpha), K)

  expect_equal(hds_priors$alpha, test_alpha)

})
