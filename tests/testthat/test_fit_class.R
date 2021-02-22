test_that("new_mcmc_fit and new_optim_fit work", {
  expect_s3_class(new_mcmc_fit(2, 2, 2, 2), c("mcmc_fit", "rater_fit"))
  expect_s3_class(new_optim_fit(2, 2, 2, 2), c("optim_fit", "rater_fit"))
})

test_that("print works for rater_fit objects", {
  expect_output(
    print(ds_fit),
    "Bayesian Dawid and Skene Model with MCMC draws"
  )

  expect_output(
    print(hds_fit),
    "Bayesian Hierarchical Dawid and Skene Model with MCMC draws"
  )
})

test_that("is.mcmc_fit works", {

  test_fit <- 2
  expect_false(is.rater_fit(test_fit))
  expect_false(is.mcmc_fit(test_fit))

  class(test_fit) <- c("mcmc_fit", "rater_fit")
  expect_true(is.mcmc_fit(test_fit))
  expect_true(is.rater_fit(test_fit))
})

test_that("plot.fit dispatches correctly", {

  # Why does this test not compare the plots directly?
  # Because of the internals of {rater} the plots are created in different
  # environments causing testthat 3e and R 4.1 to fail. In any case testing
  # the geoms is probably sufficient here as we are really testing the sanity
  # of the switch statement controlling plot dispatch.

  plot_theta_p <- plot_theta(ds_fit)
  plot_p <- plot(ds_fit, pars = "theta")
  expect_equal(get_geoms(plot_p), get_geoms(plot_theta_p))

  plot_class_probs_p <- plot_theta(ds_fit)
  plot_p <- plot(ds_fit, pars = "latent_class")
  expect_equal(get_geoms(plot_p), get_geoms(plot_class_probs_p))

  plot_pi_p <- plot_pi(ds_fit)
  plot_p <- plot(ds_fit, pars = "pi")
  expect_equal(get_geoms(plot_p), get_geoms(plot_pi_p))
})

test_that("as_mcmc.list works", {
  expect_error(as_mcmc.list(ds_fit_optim))
  expect_error(as_mcmc.list(2))
  expect_true(coda::is.mcmc.list(as_mcmc.list(ds_fit)))
})

test_that("prior_summary works", {
  expect_equal(prior_summary(ds_fit), ds_fit$model)
})

test_that("get_stanfit works", {
  expect_equal(get_stanfit(ds_fit), ds_fit$samples)
  expect_equal(get_stanfit(ds_fit_optim), ds_fit_optim$estimates)
  expect_error(get_stanfit(2))
})

test_that("summary works", {
  expect_output(summary(ds_fit))
  expect_output(summary(ds_fit_optim))
  expect_output(summary(ds_fit_grouped))

  expect_output(summary(ccds_fit))
  expect_output(summary(hds_fit))
})
