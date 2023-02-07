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

test_that("loo_compare works (smoke test)", {
  loo_ds <- suppressWarnings(loo(ds_fit))
  loo_ccds <- suppressWarnings(loo(ccds_fit))
  expect_no_error(loo_compare(loo_ds, loo_ccds))
})

test_that("waic errors appropriately", {
  expect_error(
    waic(ds_fit_grouped),
    "waic is not supported for models fit using grouped data."
  )
  expect_error(
    waic(ds_fit_optim),
    "waic cannot be calculated for models fit using optimisation."
  )
})

test_that("waic works (smoke test)", {
  # Sensitive to sampling.
  skip_on_cran()

  expect_warning(waic(ds_fit))
  expect_warning(waic(ccds_fit))
  expect_warning(waic(hds_fit))
})

test_that("waic output is the right format", {
  waic_res <- suppressWarnings(waic(ds_fit))
  expect_s3_class(waic_res, c("waic", "loo"))
})
