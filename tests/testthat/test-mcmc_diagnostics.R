test_that("mcmc_diagnostics works", {

  mcmc_diags_pi <- mcmc_diagnostics(ds_fit, pars = "pi")
  expect_equal(nrow(mcmc_diags_pi), K)
  expect_type(mcmc_diags_pi, "double")
  expect_equal(colnames(mcmc_diags_pi), c("Rhat", "ess_bulk"))

  mcmc_diags_theta <- mcmc_diagnostics(ds_fit, pars = "theta")
  expect_equal(nrow(mcmc_diags_theta), J * K * K )
  expect_type(mcmc_diags_theta, "double")
  expect_equal(colnames(mcmc_diags_theta), c("Rhat", "ess_bulk"))

  expect_error(mcmc_diagnostics(ds_fit_optim))
  expect_error(mcmc_diagnostics(ds_fit, pars = "z"))
})
