test_that("point estiamte output is named", {
  all_pars <- point_estimate(ds_fit)
  expect_named(all_pars, c("pi", "theta", "z"))
  just_pi <- point_estimate(ds_fit, pars = "pi")
  expect_named(just_pi, "pi")
})

test_that("point estimate for theta errors appropriatly", {
  expect_snapshot(point_estimate(hds_fit, pars = "theta"), error = TRUE)
})

test_that("validate_which error appropriatly", {
  expect_error(
    validate_which("which", 2),
    "which must be a positive length numeric vector"
  )
  expect_error(
    validate_which(numeric(0), 2),
    "which must be a positive length numeric vector"
  )
  expect_error(
    validate_which(1:9, 6),
    "All numbers in `which` must be drawn from 1:6"
  )
})

test_that("point_estimate output for pi has correct form", {
  K <- 4
  K_caries <- 2

  out <- point_estimate(ds_fit, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

  out <- point_estimate(ds_fit_optim, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

  out <- point_estimate(ds_fit_grouped, pars = "pi")$pi
  expect_equal(length(out), K_caries)
  expect_equal(sum(out), 1)

  out <- point_estimate(ds_fit_grouped_optim, pars = "pi")$pi
  expect_equal(length(out), K_caries)
  expect_equal(sum(out), 1)

  out <- point_estimate(ccds_fit, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

  out <- point_estimate(ccds_fit_optim, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

  out <- point_estimate(hds_fit, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

  out <- point_estimate(hds_fit_optim, pars = "pi")$pi
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)
})

test_that("point estimate output for z has the correct form", {
  K <- 4
  K_caries <- 2

  I <- 45
  I_caries <- 3859

  out <- point_estimate(ds_fit, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in% 1:K))

  out <- point_estimate(ds_fit_optim, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in% 1:K))

  out <- point_estimate(ds_fit_grouped, pars = "z")$z
  expect_equal(length(out), I_caries)
  expect_true(all(out %in%  1:K_caries))

  out <- point_estimate(ds_fit_grouped_optim, pars = "z")$z
  expect_equal(length(out), I_caries)
  expect_true(all(out %in%  1:K_caries))

  out <- point_estimate(ccds_fit, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in%  1:K))

  out <- point_estimate(ccds_fit_optim, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in%  1:K))

  out <- point_estimate(hds_fit, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in%  1:K))

  out <- point_estimate(hds_fit_optim, pars = "z")$z
  expect_equal(length(out), I)
  expect_true(all(out %in%  1:K))
})

test_that("class_probabilites output has correct form", {
  K <- 4
  K_caries <- 2

  I <- 45
  I_caries <- 3859

  out <- class_probabilities(ds_fit)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  out <- class_probabilities(ds_fit_optim)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  out <- class_probabilities(ds_fit_grouped)
  expect_equal(dim(out), c(I_caries, K_caries))
  expect_equal(rowSums(out), rep(1, I_caries))

  out <- class_probabilities(ds_fit_grouped_optim)
  expect_equal(dim(out), c(I_caries, K_caries))
  expect_equal(rowSums(out), rep(1, I_caries))

  out <- class_probabilities(ccds_fit)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  out <- class_probabilities(ccds_fit_optim)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  out <- class_probabilities(hds_fit)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  out <- class_probabilities(hds_fit_optim)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))
})

test_that("theta point_estimate for long DS (MCMC + optimisation) is correct", {
  K <- 4

  ds_mcmc_out <- point_estimate(ds_fit, pars = "theta")$theta
  expect_true(is.array(ds_mcmc_out))
  apply(ds_mcmc_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ds_optim_out <- point_estimate(ds_fit_optim, pars = "theta")$theta
  expect_equal(is.array(ds_optim_out), TRUE)
  apply(ds_optim_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))
})

test_that("theta point_estimate for grouped DS (MCMC + optimisation) is correct", {
  K <- 2

  ds_mcmc_grouped_out <- point_estimate(ds_fit_grouped, pars = "theta")$theta
  expect_true(is.array(ds_mcmc_grouped_out))
  apply(ds_mcmc_grouped_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ds_optim_grouped_out <- point_estimate(ds_fit_grouped_optim, pars = "theta")$theta
  expect_equal(is.array(ds_optim_grouped_out), TRUE)
  apply(ds_optim_grouped_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))
})

test_that("theta point_esimate for CCDS(MCMC + optimsation) has correct form", {
  J <- 5
  K <- 4

  ccds_mcmc_out <- point_estimate(ccds_fit, pars = "theta")[[1]]
  expect_true(is.array(ccds_mcmc_out))
  expect_equal(dim(ccds_mcmc_out), c(J, K, K))
  # Test that all the off diagonal elements are equal.
  expect_equal(var(ccds_mcmc_out[1, 1, -1]), 0)
  apply(ccds_mcmc_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ccds_optim_out <- point_estimate(ccds_fit_optim, pars = "theta")[[1]]
  expect_true(is.array(ccds_optim_out))
  expect_equal(dim(ccds_optim_out), c(J, K, K))
  expect_equal(var(ccds_optim_out[1, 1, -1]), 0)
  apply(ccds_optim_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))
})
