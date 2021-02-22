test_that("loo errors appropriately", {
  expect_error(
    loo(ds_fit_grouped),
    "loo is not supported for models fit using grouped data."
  )
  expect_error(
    loo(ds_fit_optim),
    "loo cannot be calculated for models fit using optimisation."
  )
})

test_that("loo works (smoke test)", {
  # Sensitive to sampling.
  skip_on_cran()

  expect_warning(loo(ds_fit))
  expect_warning(loo(ccds_fit))
  expect_warning(loo(hds_fit))
})

test_that("loo output is the right format", {
  loo_res <- suppressWarnings(loo(ds_fit))
  expect_s3_class(loo_res, "loo")
})

