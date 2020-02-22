context("extract")

test_that("extract_raters error appropriatly", {

  expect_error(
    extract_theta(hds_fit),
    "Rater metrics cannot be extracted from the Hierachical Dawid and Skene model."
  )
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

test_that("extract_pi output has correct form", {
  # mcmc
  out <- extract_pi(ds_fit)
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)
  # optim
  out <- extract_pi(ds_fit_optim)
  expect_equal(length(out), K)
  expect_equal(sum(out), 1)
  # table
  out <- extract_pi(ds_fit_table)
  expect_equal(length(out), K_caries)
  expect_equal(sum(out), 1)
})

test_that("extract_z output has correct form", {
  # mcmc
  out <- extract_z(ds_fit)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))
  # optim
  out <- extract_z(ds_fit_optim)
  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))
  # table
  out <- extract_z(ds_fit_table)
  expect_equal(dim(out), c(I_caries, K_caries))
  expect_equal(rowSums(out), rep(1, I_caries))
})


test_that("extract_theta (mcmc) output has correct form", {

  ds_out <- extract_theta(ds_fit)
  # Does it have the right form?
  expect_true(is.array(ds_out))
  # Is it a probability?
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ccds_out <- extract_theta(ccds_fit)
  expect_true(is.array(ccds_out))
  expect_equal(dim(ccds_out), c(J, K, K))
  # Test that all the off diagonal elements are equal
  expect_equal(var(ccds_out[1, 1, -1]), 0)
  apply(ccds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))
})

test_that("extract_theta (optim) output has correct form", {

  ds_out <- extract_theta(ds_fit_optim)
  expect_equal(is.array(ds_out), TRUE)
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  ccds_out <- extract_theta(ccds_fit)
  expect_true(is.array(ccds_out))
  expect_equal(dim(ccds_out), c(J, K, K))
  expect_equal(var(ccds_out[1, 1, -1]), 0)
  apply(ccds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

})

test_that("extract_theta (table) output has correct form", {

  ds_out <- extract_theta(ds_fit_table)

  # dawid skene

  # form
  expect_equal(is.array(ds_out), TRUE)

  # is probability
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K_caries)))

})

test_that("extract_ function are equivalent", {
  expect_equal(extract_theta(ds_fit), extract_raters(ds_fit))
  expect_equal(extract_z(ds_fit), extract_latent_class(ds_fit))
  expect_equal(extract_pi(ds_fit), extract_prevalence(ds_fit))
})





