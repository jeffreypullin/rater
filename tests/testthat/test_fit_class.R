
test_that("print works for fit", {

  expect_output(print(ds_fit),
                "Bayesian Dawid and Skene Model with MCMC draws")
  expect_output(print(hds_fit),
                "Bayesian Hierarchical Dawid and Skene Model with MCMC draws")

})

test_that("is.mcmc_fit works", {

  test_fit <- 2
  expect_equal(is.rater_fit(test_fit), FALSE)
  expect_equal(is.mcmc_fit(test_fit), FALSE)

  class(test_fit) <- c("mcmc_fit", "rater_fit")
  expect_equal(is.mcmc_fit(test_fit), TRUE)
  expect_equal(is.rater_fit(test_fit), TRUE)
})

# still unsure about best API here

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

test_that("can plot multiple parameters", {

  expect_error(plot(ds_fit, pars = c("theta", "pi")), NA)

})

test_that("as_mcmc.list works", {

  expect_error(as_mcmc.list(ds_fit_optim))
  expect_error(as_mcmc.list(2))
  expect_true(coda::is.mcmc.list(as_mcmc.list(ds_fit)))

})


