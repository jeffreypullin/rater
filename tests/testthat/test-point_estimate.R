test_that("point estiamte output is named", {
  all_pars <- point_estimate(ds_fit)
  expect_named(all_pars, c("pi", "theta", "z"))
  just_pi <- point_estimate(ds_fit, pars = "pi")
  expect_named(just_pi, "pi")
})


test_that("point estimate for theta errors appropriatly", {

  # We used to test the error message here but it was too fiddly.
  expect_error(point_estimate(hds_fit, pars = "theta"))

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

test_that("point estimate output for pi has correct form", {
  # mcmc
  out <- point_estimate(ds_fit, pars = "pi")[[1]]
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)
  # optim
  out <- point_estimate(ds_fit_optim, pars = "pi")[[1]]
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)
  # grouped
  out <- point_estimate(ds_fit_grouped, pars = "pi")[[1]]
  expect_equal(length(out), K_caries)
  expect_equal(sum(out), 1)
})

test_that("point estimate output for z has the correct form", {
  # MCMC
  out <- point_estimate(ds_fit, pars = "z")[[1]]
  expect_equal(length(out), I)
  expect_true(all(out %in% 1:K))

  # Optimisation
  out <- point_estimate(ds_fit_optim, pars = "z")[[1]]
  expect_equal(length(out), I)
  expect_true(all(out %in% 1:K))

  # Grouped data MCMC
  out <- point_estimate(ds_fit_grouped, pars = "z")[[1]]
  expect_equal(length(out), I_caries)
  expect_true(all(out %in%  1:K_caries))

  # Grouped data optim
  out <- point_estimate(ds_fit_grouped_optim, pars = "z")[[1]]
  expect_equal(length(out), I_caries)
  expect_true(all(out %in%  1:K_caries))
})

test_that("class_probabilites output has correct form", {
  # MCMC
  out <- class_probabilities(ds_fit)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  # Optimisation
  out <- class_probabilities(ds_fit_optim)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

  # Grouped data MCMC
  out <- class_probabilities(ds_fit_grouped)
  expect_equal(dim(out), c(I_caries, K_caries))
  expect_equal(rowSums(out), rep(1, I_caries))
})


test_that("point estimate (mcmc) output for theta has correct form", {

  ds_out <- point_estimate(ds_fit, pars = "theta")[[1]]
  # Does it have the right form?
  expect_true(is.array(ds_out))
  # Is it a probability?
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ccds_out <- point_estimate(ccds_fit, pars = "theta")[[1]]
  expect_true(is.array(ccds_out))
  expect_equal(dim(ccds_out), c(J, K, K))
  # Test that all the off diagonal elements are equal
  expect_equal(var(ccds_out[1, 1, -1]), 0)
  apply(ccds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))
})

test_that("point esimate (optim) output for theta has correct form", {

  ds_out <- point_estimate(ds_fit_optim, pars = "theta")[[1]]
  expect_equal(is.array(ds_out), TRUE)
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ccds_out <- point_estimate(ccds_fit, pars = "theta")[[1]]
  expect_true(is.array(ccds_out))
  expect_equal(dim(ccds_out), c(J, K, K))
  expect_equal(var(ccds_out[1, 1, -1]), 0)
  apply(ccds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

})

test_that("extract_theta (grouped) output has correct form", {

  ds_out <- point_estimate(ds_fit_grouped, pars = "theta")[[1]]

  # dawid skene

  # form
  expect_equal(is.array(ds_out), TRUE)

  # is probability
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K_caries)))

})
