context("plotting")

test_that("plot_prevalance output has correct type", {

  ds_plot <- plot_prevalance(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomBar", "GeomText"))

})

test_that("plot_raters output has correct type", {

  ds_plot <- plot_raters(ds_fit)
  multi_plot <- plot_raters(multi_fit)

  expect_equal(get_facet_dim(ds_plot), 5)
  expect_equal(get_facet_dim(multi_plot), 1)

  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})

test_that("plot_latent_class output has correct type", {

  ds_plot <- plot_latent_class(ds_fit)
  expect_equal(get_geoms(ds_plot), c("GeomTile", "GeomText"))

})
