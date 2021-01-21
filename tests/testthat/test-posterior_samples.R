test_that("posterior_samples works", {

  # We get a list if length(pars) > 1
  expect_type(posterior_samples(ds_fit, pars = c("pi", "theta")), "list")
  expect_named(posterior_samples(ds_fit, pars = c("pi", "theta")),
               c("pi", "theta"))

  # We error if the user requests draws from z
  expect_error(posterior_samples(ds_fit, pars = c("z")))

  # Or if they pass an invalid pararmaeter
  expect_error(posterior_samples(ds_fit, pars = c("nonsense")))
})
