context("fit_class")

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

  raters_plot <- plot_theta(ds_fit)
  expect_equal(plot(ds_fit, pars = "theta"), raters_plot)

  latent_class_plot <- plot_class_probabilities(ds_fit)
  expect_equal(plot(ds_fit, pars = "latent_class"), latent_class_plot)

  prevalence_plot <- plot_pi(ds_fit)
  expect_equal(plot(ds_fit, pars = "pi"), prevalence_plot)

})

test_that("can plot multiple parameters", {

  raters_plot <- plot_theta(ds_fit)
  prevalence_plot <- plot_pi(ds_fit)
  expect_equal(plot(ds_fit, pars = c("theta", "pi")),
              list(raters_plot, prevalence_plot))

})


