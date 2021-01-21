test_that("loo works", {

  expect_error(loo(ds_fit), NA)
  expect_error(loo(ds_fit_grouped),
               "loo is not supported for models fit using grouped data.")
  expect_error(loo(ds_fit_optim),
               "loo cannot be calculated for models fit using optimisation.")

})
