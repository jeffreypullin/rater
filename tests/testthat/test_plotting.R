
test_that("plot_prevalence output has correct type", {

  ds_plot <- plot_pi(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomPoint", "GeomErrorbar"))

})

test_that("can plot pi with optim_fit", {

  expect_error(plot(ds_fit_optim, pars = "pi"), NA)

})


test_that("plot_raters output has correct type", {

  ds_plot <- plot_theta(ds_fit)
  expect_equal(get_facet_dim(ds_plot), 5)
  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})

test_that("plot_latent_class output has correct type", {

  ds_plot <- plot_class_probabilities(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})
