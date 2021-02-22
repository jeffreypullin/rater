test_that("posterior_predict errors for the HDS model", {
  new_data <- data.frame(item = rep(1:2, each = J), rater = rep(1:J, 2))
  expect_snapshot(posterior_predict(hds_fit, new_data), error = TRUE)
  expect_snapshot(posterior_predict(hds_fit_optim, new_data), error = TRUE)
})

test_that("posterior_predict works for all other model types (smoke test)", {
  J <- 5
  new_data <- data.frame(item = rep(1:2, each = J), rater = rep(1:J, 2))

  expect_ok(posterior_predict(ds_fit, new_data))
  expect_ok(posterior_predict(ccds_fit, new_data))

  expect_ok(posterior_predict(ds_fit_optim, new_data))
  expect_ok(posterior_predict(ccds_fit_optim, new_data))

  expect_ok(posterior_predict(ds_fit_grouped, new_data))
  expect_ok(posterior_predict(ds_fit_grouped_optim, new_data))
})

test_that("posterior_predict respects the seed", {
  new_data <- data.frame(item = rep(1:2, each = J), rater = rep(1:J, 2))
  expect_equal(posterior_predict(ds_fit, new_data, seed = 430),
               posterior_predict(ds_fit, new_data, seed = 430))
})

test_that("posterior_predict validates new_data correctly", {

  expect_error(
    posterior_predict(ds_fit, new_data = data.frame(a = 1, b = 2, c = 3)),
    "`new_data` must have two columns 'item' and 'rater'"
  )
  expect_error(
    posterior_predict(ds_fit, new_data = data.frame(raters = 1, items = 3)),
    "`new_data` must have two columns 'item' and 'rater'"
  )

  wrong_new_data <- data.frame(item = rep(1:2, each = 6), rater = rep(1:6, 2))
  expect_error(
    posterior_predict(ds_fit, wrong_new_data),
    "The number of raters in the fitted and new data must match"
  )
})
