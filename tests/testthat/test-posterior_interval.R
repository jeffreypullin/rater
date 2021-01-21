test_that("posterior_interval works", {

  all_intervals <- posterior_interval(ds_fit)
  expect_type(all_intervals, "double")
  expect_equal(dim(all_intervals), c(84, 2))

  pi_intervals <- posterior_interval(ds_fit, pars = "pi")
  expect_equal(dim(pi_intervals), c(4, 2))

  expect_error(posterior_interval(ds_fit, pars = "z"))
})
