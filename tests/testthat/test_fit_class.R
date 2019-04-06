context("fit_class")

test_that("summary works for fit", {

  expect_output(summary(ds_fit), "Bayesian Dawid and Skene Model with MCMC draws")

  # Problems with the () - actual value has \(
  #expect_output(summary(multi_model), "Bayesian Multinomial (Annotator pooled) Model")

  expect_output(summary(hds_fit), "Bayesian Hierarchical Dawid and Skene Model with MCMC draws")

})

test_that("is.mcmc_fit works", {

  test_fit <- 2
  expect_equal(is.rater_fit(test_fit), FALSE)
  expect_equal(is.mcmc_fit(test_fit), FALSE)

  class(test_fit) <- c("mcmc_fit", "rater_fit")
  expect_equal(is.mcmc_fit(test_fit), TRUE)
  expect_equal(is.rater_fit(test_fit), TRUE)
})

# We now have a default option for this
# test_that("plot.fit errors properly", {
#   expect_error(plot(ds_fit), "The type of plot must be specified")
# })

# still unsure about best API here

test_that("plot.fit dispatches correctly", {

  raters_plot <- plot_theta(ds_fit)
  expect_equal(plot(ds_fit, type = "theta"), raters_plot)

  latent_class_plot <- plot_z(ds_fit)
  expect_equal(plot(ds_fit, type = "z"), latent_class_plot)

  prevalance_plot <- plot_pi(ds_fit)
  expect_equal(plot(ds_fit, type = "pi"), prevalance_plot)

})


