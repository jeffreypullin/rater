
test_that("verbose flag works", {

  expect_message(
    suppressWarnings(
      rater(anesthesia, "dawid_skene",
            chains = 1, iter = 200, verbose = FALSE)
      ),
    NA
  )

})


test_that("passing model as string works", {

  # This was failing previously because the check of whether the model and
  # format are compatible requires an *actual* model, so we have to validate
  # and convert string -> model object before validating.
  expect_error(rater(caries, "dawid_skene", method = "optim",
                     data_format = "grouped"),
               NA)

  # These were causing a non-zero return code warning but I can't reproduce
  # it outside of testthat...
  fit_model <- suppressWarnings(
    rater(anesthesia, dawid_skene(), method = "optim")
  )
  fit_string <- suppressWarnings(
    rater(anesthesia, "dawid_skene", method = "optim")
  )

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
  J <- anesthesia_list$J
  ds_priors <- parse_priors(dawid_skene(), K, J)

  # construct the default priors
  default_alpha <- rep(3, K)

  N <- 8
  p <- 0.6
  on_diag <- N * p
  off_diag <- N * (1 - p) / (K - 1)
  beta_slice <- matrix(off_diag, nrow = K, ncol = K)
  diag(beta_slice) <- on_diag
  default_beta <- array(dim = c(J, K, K))
  for (j in 1:J) {
    default_beta[j, , ] <- beta_slice
  }

  expect_equal(ds_priors$alpha, default_alpha)
  expect_equal(ds_priors$beta, default_beta)

  # test non-default priors
  test_alpha <- rep(9, K)
  test_beta_mat <- matrix(17, nrow = K, ncol = K)
  test_beta_array <- array(dim = c(J, K, K))
  for (j in 1:J) {
    test_beta_array[j, , ] <- test_beta_mat
  }

  ds_priors_mat <- parse_priors(dawid_skene(alpha = test_alpha,
                                            beta = test_beta_mat),
                                K,
                                J)

  expect_equal(ds_priors_mat$alpha, test_alpha)
  expect_equal(ds_priors_mat$beta, test_beta_array)

  ds_priors_array <- parse_priors(dawid_skene(alpha = test_alpha,
                                              beta = test_beta_array),
                                  K,
                                  J)

  expect_equal(ds_priors_array$beta, test_beta_array)

  # default priors
  hds_priors <- parse_priors(hier_dawid_skene(), K, J)
  expect_equal(hds_priors$alpha, default_alpha)

  # non-default priors
  hds_priors <- parse_priors(hier_dawid_skene(alpha = test_alpha), K, J)
  expect_equal(hds_priors$alpha, test_alpha)

  test_beta_1 <- rep(1, K)
  test_beta_2 <- rep(98, K)
  # Class conditional model
  ccds_priors <- parse_priors(
    class_conditional_dawid_skene(
      alpha = test_alpha,
      beta_1 = test_beta_1,
      beta_2 = test_beta_2),
    K,
    J
  )

  expect_equal(ccds_priors$alpha, test_alpha)
  expect_equal(ccds_priors$beta_1, test_beta_1)
  expect_equal(ccds_priors$beta_2, test_beta_2)
})
