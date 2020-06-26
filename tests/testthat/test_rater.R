context("rater")

test_that("passing model as string works", {

  # This was failing previously because the check of whether the model and
  # format are compatible requires an *actual* model, so we have to validate
  # and convert string -> model object before validating.
  expect_error(rater(caries, "dawid_skene", method = "optim",
                     data_format = "grouped"),
               NA)

  fit_model <- rater(anesthesia, dawid_skene(), method = "optim")
  fit_string <- rater(anesthesia, "dawid_skene", method = "optim")

  expect_equal(fit_model, fit_string)
})


test_that("rater infernce is 'correct'", {
  # TODO This is a stopgap solution designed to detect large changes in
  # behaviour. In future, it would be great to have a full framework to assess
  # the the performance of the inference.
  pi_est <- point_estimate(ds_fit_optim, pars = "pi")[[1]]
  # Correct value is 0.41.
  expect_lt(pi_est[[2]], 0.45)
  expect_gt(pi_est[[2]], 0.35)
})

test_that("rater returns objects of the correct type", {

  expect_equal(is.rater_fit(ds_fit), TRUE)
  expect_equal(is.mcmc_fit(ds_fit), TRUE)

})

test_that("rater errors correctly", {

  expect_error(rater(anesthesia, "not_a_proper_model"))
  expect_error(rater(caries, hier_dawid_skene(), data_format = "grouped"))
  expect_error(rater(1:10, dawid_skene()))
  expect_error(rater(data.frame(1, 2), dawid_skene()))
  expect_error(rater(data.frame(item = 1, rater = 1, ratingg = 1),
                     dawid_skene()))
  expect_error(rater(data.frame(anything = 1, not_n = 1), dawid_skene(),
                     data_format = "grouped"))

})

test_that("parse priors are correct", {

  # dawid skene model

  anesthesia_list <- as_stan_data(anesthesia, "long")

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
  test_beta_1 <- rep(1, K)
  test_beta_2 <- rep(98, K)
  ds_priors <- parse_priors(dawid_skene(alpha = test_alpha, beta = test_beta), K)

  expect_equal(ds_priors$alpha, test_alpha)
  expect_equal(ds_priors$beta, test_beta)

  # default priors
  hds_priors <- parse_priors(hier_dawid_skene(), K)
  expect_equal(hds_priors$alpha, default_alpha)

  # non-default priors
  hds_priors <- parse_priors(hier_dawid_skene(alpha = test_alpha), K)
  expect_equal(hds_priors$alpha, test_alpha)

  # Class conditional model
  ccds_priors <- parse_priors(
    class_conditional_dawid_skene(
      alpha = test_alpha,
      beta_1 = test_beta_1,
      beta_2 = test_beta_2),
    K
  )

  expect_equal(ccds_priors$alpha, test_alpha)
  expect_equal(ccds_priors$beta_1, test_beta_1)
  expect_equal(ccds_priors$beta_2, test_beta_2)
})
