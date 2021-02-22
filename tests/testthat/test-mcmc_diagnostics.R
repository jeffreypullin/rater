test_that("mcmc_diagnostics error appropriatly", {
  expect_error(
    mcmc_diagnostics(ds_fit_optim),
    "Cannot extract MCMC diagnositcs from a optimisation fit."
  )
  expect_error(
    mcmc_diagnostics(ds_fit, pars = "z"),
    "Cannot extract MCMC diagnostics for the latent class."
  )
})

test_that("mcmcm_diagnostics errors usefully with the hierarchical DS model", {
  # Too hard to get error as a string...
  expect_snapshot(mcmc_diagnostics(hds_fit), error = TRUE)
})

test_that("MCMC diagnostics for pi have the correct form", {
  K <- 4
  ds_mcmc_diags_pi <- mcmc_diagnostics(ds_fit, pars = "pi")
  expect_equal(nrow(ds_mcmc_diags_pi), K)
  expect_type(ds_mcmc_diags_pi, "double")
  expect_equal(colnames(ds_mcmc_diags_pi), c("Rhat", "ess_bulk"))

  ccds_mcmc_diags_pi <- mcmc_diagnostics(ccds_fit, pars = "pi")
  expect_equal(nrow(ccds_mcmc_diags_pi), K)
  expect_type(ccds_mcmc_diags_pi, "double")
  expect_equal(colnames(ccds_mcmc_diags_pi), c("Rhat", "ess_bulk"))

  hds_mcmc_diags_pi <- mcmc_diagnostics(hds_fit, pars = "pi")
  expect_equal(nrow(hds_mcmc_diags_pi), K)
  expect_type(hds_mcmc_diags_pi, "double")
  expect_equal(colnames(hds_mcmc_diags_pi), c("Rhat", "ess_bulk"))
})

test_that("MCMC diagnostics for theta have the correct form", {
  K <- 4
  J <- 5
  ds_mcmc_diags_theta <- mcmc_diagnostics(ds_fit, pars = "theta")
  expect_equal(nrow(ds_mcmc_diags_theta), J * K * K)
  expect_type(ds_mcmc_diags_theta, "double")
  expect_equal(colnames(ds_mcmc_diags_theta), c("Rhat", "ess_bulk"))

  ccds_mcmc_diags_theta <- mcmc_diagnostics(ccds_fit, pars = "theta")
  expect_equal(nrow(ccds_mcmc_diags_theta), J * K * K)
  expect_type(ccds_mcmc_diags_theta, "double")
  expect_equal(colnames(ccds_mcmc_diags_theta), c("Rhat", "ess_bulk"))
})

