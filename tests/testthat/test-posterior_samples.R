test_that("posterior_samples works", {

  # We get a list if length(pars) > 1
  expect_type(posterior_samples(ds_fit, pars = c("pi", "theta")), "list")
  expect_named(posterior_samples(ds_fit, pars = c("pi", "theta")),
               c("pi", "theta"))

  # We error if the user requests draws from z
  expect_error(posterior_samples(ds_fit, pars = c("z")))

  # Or if they pass an invalid parameter
  expect_error(posterior_samples(ds_fit, pars = c("nonsense")))
})

test_that("posterior_samples returns full theta for class conditional model", {

  # 100 is the number of post-warmup samples in the tests.
  expect_equal(dim(posterior_samples(ccds_fit)$theta), c(100, J, K, K))

})
