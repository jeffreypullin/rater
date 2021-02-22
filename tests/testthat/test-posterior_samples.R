test_that("posterior_samples works (smoke test)", {
  expect_ok(posterior_samples(ds_fit))
  expect_ok(posterior_samples(ccds_fit))
  expect_ok(posterior_samples(ds_fit_grouped))
})

test_that("HDS errors informatively", {
  expect_snapshot(posterior_samples(hds_fit), error = TRUE)
})

test_that("posterior_samples output has the correct form", {
  expect_type(posterior_samples(ds_fit, pars = c("pi", "theta")), "list")
  expect_length(posterior_samples(ds_fit, pars = "pi"), 1)
  expect_named(
    posterior_samples(ds_fit, pars = c("pi", "theta")),
    c("pi", "theta")
  )
})

test_that("posterior_samples errors correctly", {
  expect_error(
    posterior_samples(ds_fit, pars = c("z")),
    "Cannot return draws for marginalised discrete parameter"
  )

  expect_error(posterior_samples(ds_fit, pars = c("nonsense")))
})

test_that("posterior_samples returns full theta for class conditional model", {
  # 100 is the number of post-warmup samples in the tests.
  expect_equal(dim(posterior_samples(ccds_fit)$theta), c(100, J, K, K))
})
