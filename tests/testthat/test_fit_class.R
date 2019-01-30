context("fit_class")

test_that("summary works for fit", {

  expect_output(summary(ds_fit), "Bayesian Dawid and Skene Model with MCMC draws")

  # Problems with the () - actual value has \(
  #expect_output(summary(multi_model), "Bayesian Multinomial (Annotator pooled) Model")

  expect_output(summary(hds_fit), "Bayesian Hierarchical Dawid and Skene Model with MCMC draws")

})

test_that("is.fit works", {

  test_fit <- 2
  expect_equal(is.fit(test_fit), FALSE)
  class(test_fit) <- "fit"
  expect_equal(is.fit(test_fit), TRUE)

})

test_that("plot.fit errors properly", {
  expect_error(plot(ds_fit), "The type of plot must be specified")
})

test_that("plot.fit dispatches correctly", {

  raters_plot <- plot_raters(ds_fit)
  expect_equal(plot(ds_fit, type = "raters"), raters_plot)
  expect_equal(plot(ds_fit, type = "theta"), raters_plot)

  latent_class_plot <- plot_latent_class(ds_fit)
  expect_equal(plot(ds_fit, type = "latent_class"), latent_class_plot)
  expect_equal(plot(ds_fit, type = "z"), latent_class_plot)

  prevalance_plot <- plot_prevalance(ds_fit)
  expect_equal(plot(ds_fit, type = "prevalance"), prevalance_plot)
  expect_equal(plot(ds_fit, type = "pi"), prevalance_plot)

})

test_that("plot.extract errors properly", {
  expect_error(extract(ds_fit), "The param to extract must be specified")
})


test_that("extract.fit dispatches correctly", {

  raters_param <- extract_raters(ds_fit)
  expect_equal(extract(ds_fit, param = "raters"), raters_param)
  expect_equal(extract(ds_fit, param = "theta"), raters_param)

  latent_class_param <- extract_latent_class(ds_fit)
  expect_equal(extract(ds_fit, param = "latent_class"), latent_class_param)
  expect_equal(extract(ds_fit, param = "z"), latent_class_param)

  prevalance_param <- extract_prevalance(ds_fit)
  expect_equal(extract(ds_fit, param = "prevalance"), prevalance_param)
  expect_equal(extract(ds_fit, param = "pi"), prevalance_param)

})

