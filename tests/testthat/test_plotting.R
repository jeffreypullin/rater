test_that("Plotting works for long data models fit with MCMC (smoke test)", {
  expect_ok(plot(ds_fit, "pi"))
  expect_ok(plot(ds_fit, "theta"))
  expect_ok(plot(ds_fit, "theta", theta_plot_type = "points"))
  expect_ok(plot(ds_fit, "class_probabilities"))

  expect_ok(plot(ccds_fit, "pi"))
  expect_ok(plot(ccds_fit, "theta"))
  expect_ok(plot(ccds_fit, "theta", theta_plot_type = "points"))
  expect_ok(plot(ccds_fit, "class_probabilities"))

  expect_ok(plot(hds_fit, "pi"))
  expect_ok(plot(hds_fit, "theta"))
  expect_ok(plot(hds_fit, "theta", theta_plot_type = "points"))
  expect_ok(plot(hds_fit, "class_probabilities"))
})

test_that("Plotting works for long data models fit with optimisation (smoke test)", {
  expect_ok(plot(ds_fit_optim, "pi"))
  expect_ok(plot(ds_fit_optim, "theta"))
  expect_error(plot(ds_fit_optim, "theta", theta_plot_type = "points"))
  expect_ok(plot(ds_fit_optim, "class_probabilities"))

  expect_ok(plot(ccds_fit_optim, "pi"))
  expect_ok(plot(ccds_fit_optim, "theta"))
  expect_error(plot(ccs_fit_optim, "theta", theta_plot_type = "points"))
  expect_ok(plot(ccds_fit_optim, "class_probabilities"))

  expect_ok(plot(hds_fit_optim, "pi"))
  expect_ok(plot(hds_fit_optim, "theta"))
  expect_error(plot(hds_fit_optim, "theta", theta_plot_type = "points"))
  expect_ok(plot(hds_fit_optim, "class_probabilities"))
})

test_that("Plotting works for grouped data Dawid-Skene (MCMC + optimisation) (smoke test)", {
  expect_ok(plot(ds_fit_grouped, "pi"))
  expect_ok(plot(ds_fit_grouped, "theta"))
  expect_ok(plot(ds_fit_grouped, "theta", theta_plot_type = "points"))
  expect_ok(plot(ds_fit_grouped, "class_probabilities"))

  expect_ok(plot(ds_fit_grouped_optim, "pi"))
  expect_ok(plot(ds_fit_grouped_optim, "theta"))
  expect_error(plot(ds_fit_grouped_option, "theta", theta_plot_type = "points"))
  expect_ok(plot(ds_fit_grouped_optim, "class_probabilities"))
})

test_that("plot_prevalence output has correct type", {
  ds_plot <- plot_pi(ds_fit)
})

test_that("plot_raters output has correct type", {
  ds_plot <- plot_theta(ds_fit)
  expect_equal(get_facet_dim(ds_plot), 5)
})

test_that("plot_latent_class output has correct type", {
  ds_plot <- plot_class_probabilities(ds_fit)
})

test_that("plot_theta_points output has correct type", {
  ds_plot <- plot_theta_points(ds_fit)
})
