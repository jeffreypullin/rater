test_that("verbose flag works", {
  expect_silent(
    suppressWarnings(
      rater(anesthesia, "dawid_skene",
            chains = 1, iter = 200, verbose = FALSE)
      )
  )
})

test_that("Passing model as string works", {

  # Unexplained warnings in the past - potentially flaky...
  skip_on_cran()

  # This was failing previously because the check of whether the model and
  # format are compatible requires an *actual* model, so we have to validate
  # and convert string -> model object before validating.
  expect_ok(
    rater(caries, "dawid_skene", method = "optim", data_format = "grouped")
  )

  fit_function <- rater(anesthesia, dawid_skene(), method = "optim")
  fit_string <- rater(anesthesia, "dawid_skene", method = "optim")
  expect_equal(fit_function, fit_string)
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
  expect_true(is.rater_fit(ds_fit))
  expect_true(is.mcmc_fit(ds_fit))
  expect_true(is.optim_fit(ds_fit_optim))
})

test_that("rater errors correctly", {
  expect_error(
    rater(anesthesia, "not_a_proper_model"),
    "Invalid model string specification."
  )
  expect_error(
    rater(caries, hier_dawid_skene(), data_format = "grouped"),
    "Grouped data can only be used with the Dawid and Skene model."
  )
  expect_error(
    rater(1:10, dawid_skene()),
    "`data` must be a data.frame or matrix."
  )
  expect_error(
    rater(data.frame(1, 2), dawid_skene()),
    "Long format `data` must have exactly three columns."
  )
  expect_error(
    rater(data.frame(item = 1, rater = 1, ratingg = 1), dawid_skene()),
    "Long `data` must have three columns with names: `rater`, `item` and `rating`."
  )
  expect_error(
    rater(data.frame(anything = 1, not_n = 1), dawid_skene(), data_format = "grouped"),
    "The last column must be named `n`."
  )

  expect_snapshot(
    rater(data.frame(item = 0, rater = 0, rating = 0), dawid_skene()),
    error = TRUE
  )

  expect_snapshot(
    rater(data.frame(thing = 0, n = 0), dawid_skene(), data_format = "grouped"),
    error = TRUE
  )
})

test_that("rater provides useful messages for probably not long data", {

  expect_error(
    suppressMessages(
      expect_message(
        rater(data.frame(1, 2, 3, 3), "dawid_skene"),
        "Is your data in wide format? Consider using `data_format = wide`."
      )
    )
  )

  expect_error(
    suppressMessages(
      expect_message(
        rater(data.frame(1, 2, 3, 31), "dawid_skene"),
        "Is your data in grouped format? Consider using `data_format = grouped`."
      )
    )
  )
})

test_that("parse_priors is correct for the Dawid-Skene model", {

  anesthesia_list <- as_stan_data(anesthesia, "long")

  K <- anesthesia_list$K
  J <- anesthesia_list$J
  ds_priors <- parse_priors(dawid_skene(), K, J)

  # Construct the default priors.
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

  test_alpha <- rep(9, K)
  test_beta_mat <- matrix(17, nrow = K, ncol = K)
  test_beta_array <- array(dim = c(J, K, K))
  for (j in 1:J) {
    test_beta_array[j, , ] <- test_beta_mat
  }

  ds_priors_mat <- parse_priors(
    dawid_skene(alpha = test_alpha, beta = test_beta_mat),
    K,
    J
  )

  expect_equal(ds_priors_mat$alpha, test_alpha)
  expect_equal(ds_priors_mat$beta, test_beta_array)

  ds_priors_array <- parse_priors(
    dawid_skene(alpha = test_alpha, beta = test_beta_array),
    K,
    J
  )

  expect_equal(ds_priors_array$beta, test_beta_array)
})

test_that("parse_priors is correct for the Hierarchical Dawid-Skene model", {
  default_alpha <- rep(3, K)
  test_alpha <- rep(9, K)

  hds_priors <- parse_priors(hier_dawid_skene(), K, J)
  expect_equal(hds_priors$alpha, default_alpha)

  hds_priors <- parse_priors(hier_dawid_skene(alpha = test_alpha), K, J)
  expect_equal(hds_priors$alpha, test_alpha)
})

test_that("parse_priors is correct for the Class conditional Dawid-Skene model", {

  test_beta_1 <- rep(1, K)
  test_beta_2 <- rep(98, K)
  test_alpha <- rep(9, K)

  ccds_priors <- parse_priors(
    class_conditional_dawid_skene(
      alpha = test_alpha,
      beta_1 = test_beta_1,
      beta_2 = test_beta_2
    ),
    K,
    J
  )

  expect_equal(ccds_priors$alpha, test_alpha)
  expect_equal(ccds_priors$beta_1, test_beta_1)
  expect_equal(ccds_priors$beta_2, test_beta_2)
})

test_that("as_stan_data handles wide data correctly", {

  wide_data <- data.frame(c(3, 2, 2), c(4, 2, 2))
  long_data <- data.frame(item = c(1, 1, 2, 2, 3, 3),
                          rater = c(1, 2, 1, 2, 1, 2),
                          rating = c(3, 4, 2, 2, 2, 2))

  expect_equal(as_stan_data(wide_data, "wide"),
               as_stan_data(long_data, "long"))
})

test_that("create_inits() works for the Dawid-Skene model", {
  anesthesia_stan_data <- as_stan_data(anesthesia, "long")
  K <- anesthesia_stan_data$K
  J <- anesthesia_stan_data$J

  pi_init <- rep(1 / K, K)
  theta_init <- array(0.2 / (K - 1), c(J, K, K))
  for (j in 1:J) {
      diag(theta_init[j, ,]) <- 0.8
  }

  expect_equal(
    create_inits(dawid_skene(), anesthesia_stan_data),
    function(n) list(theta = theta_init, pi = pi_init),
    ignore_function_env = TRUE
  )
})

test_that("create_inits() works for the class conditional Dawid-Skene model", {
  anesthesia_stan_data <- as_stan_data(anesthesia, "long")
  K <- anesthesia_stan_data$K
  J <- anesthesia_stan_data$J

  pi_init <- rep(1 / K, K)
  theta_init <- matrix(0.8, nrow = J, ncol = K)

  expect_equal(
    create_inits(class_conditional_dawid_skene(), anesthesia_stan_data),
    function(n) list(theta = theta_init, pi = pi_init),
    ignore_function_env = TRUE
  )
})

test_that("create_inits() works for the hierarchical Dawid-Skene model", {
  anesthesia_stan_data <- as_stan_data(anesthesia, "long")

  expect_equal(
    create_inits(hier_dawid_skene(), anesthesia_stan_data),
    "random"
  )
})
