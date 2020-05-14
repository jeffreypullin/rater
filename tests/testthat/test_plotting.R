context("plotting")

test_that("plot_prevalence output has correct type", {

  ds_plot <- plot_pi(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomBar", "GeomText"))

})

test_that("plot_raters output has correct type", {

  ds_plot <- plot_theta(ds_fit)
  expect_equal(get_facet_dim(ds_plot), 5)
  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})

test_that("plot_latent_class output has correct type", {

  ds_plot <- plot_z(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})

test_that("plot_z can plot matrices", {

  expect_silent(plot_z(matrix(0, nrow = 2, ncol = 2)))

})
