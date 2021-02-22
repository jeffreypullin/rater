test_that("posterior_interval pi has the correct form", {
  ds_pi_interval <- posterior_interval(ds_fit, pars = "pi")
  expect_equal(dim(ds_pi_interval), c(4, 2))
  expect_equal(colnames(ds_pi_interval), c("5%", "95%"))

  ccds_pi_interval <- posterior_interval(ccds_fit, pars = "pi")
  expect_equal(dim(ccds_pi_interval), c(4, 2))
  expect_equal(colnames(ccds_pi_interval), c("5%", "95%"))

  hds_pi_interval <- posterior_interval(hds_fit, pars = "pi")
  expect_equal(dim(hds_pi_interval), c(4, 2))
  expect_equal(colnames(hds_pi_interval), c("5%", "95%"))
})

test_that("Can change interval probability", {
  default <- posterior_interval(ds_fit, pars = "pi")

  smaller <- posterior_interval(ds_fit, pars = "pi", prob = 0.5)
  expect_lte(default[1, 1], smaller[1, 1])
  expect_gte(default[1, 2], smaller[1, 2])

  larger <- posterior_interval(ds_fit, pars = "pi", prob = 0.99)
  expect_gte(default[1, 1], larger[1, 1])
  expect_lte(default[1, 2], larger[1, 2])
})

test_that("posterior_interval for theta has the correct form", {
  J <- 5
  K <- 4

  ds_theta_interval <- posterior_interval(ds_fit, pars = "theta")
  expect_equal(dim(ds_theta_interval), c(J * K * K , 2))
  expect_equal(colnames(ds_theta_interval), c("5%", "95%"))

  ccds_theta_interval <- posterior_interval(ds_fit, pars = "theta")
  expect_equal(dim(ccds_theta_interval), c(J * K * K , 2))
  expect_equal(colnames(ccds_theta_interval), c("5%", "95%"))
})

test_that("DS and CCDS posterior_interval for theta have the same rownames", {
  ds_theta_interval <- posterior_interval(ds_fit, pars = "theta")
  ccds_theta_interval <- posterior_interval(ds_fit, pars = "theta")
  expect_equal(rownames(ds_theta_interval), rownames(ccds_theta_interval))
})

test_that("posterior_interval errors correctly", {
  expect_error(
    posterior_interval(ds_fit_optim, pars = "z"),
    "Can't calculate posterior intervals for a model fit using optimisation."
  )
  expect_error(
    posterior_interval(ds_fit, pars = "z"),
    "Cannot calculate quantiles for z"
  )
})

test_that("posterior_interval errors informatively with the HDS", {
  expect_snapshot(posterior_interval(hds_fit), error = TRUE)
})

test_that("posterior_interval orders parameters correctly", {
  correct_rownames <- sprintf("theta[1, 1, %s]", 1:K)
  expect_equal(rownames(posterior_interval(ds_fit, pars = "theta"))[1:K],
               correct_rownames)
})
