context("utils")

# TODO add different models wheen included

test_that("test get_name", {
  model <- dawid_skene()

  expect_equal(get_name(model), "Bayesian Dawid and Skene Model")
  expect_error(get_name(2), "Model type not supported")
  expect_error(get_name(list(2, 3)), "Model type not supported")

})

test_that("test get_file", {
  model <- dawid_skene()

  expect_equal(get_file(model), "dawid_skene")
  expect_error(get_name(2), "Model type not supported")
  expect_error(get_name(list(2, 3)), "Model type not supported")

})

test_that("test get_file", {
  model <- dawid_skene()

  expect_equal(get_file(model), "dawid_skene")
  expect_error(get_name(2), "Model type not supported")
  expect_error(get_name(list(2, 3)), "Model type not supported")

})

test_that("test validate_fit", {

  expect_error(validate_fit(ds_fit), NA)
  expect_error(validate_fit(2), "Cannot plot a non-fit object")

})
