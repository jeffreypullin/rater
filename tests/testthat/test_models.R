context("models")

test_that("test dawid_skene constructor", {

  model <- dawid_skene()

  # class construction
  expect_equal(is.rater_model(model), TRUE)
  expect_equal(is.dawid_skene(model), TRUE)

  # overall and internal shape
  expect_equal(length(model), 4)
  expect_equal(length(get_parameters(model)), 2)

  # defualt parameter values
  expect_equal(get_parameters(model)$alpha, NULL)
  expect_equal(get_parameters(model)$beta, NULL)

})

test_that("test hier_dawid_skene constructor", {

  model <- hier_dawid_skene()

  # class construction
  expect_equal(is.rater_model(model), TRUE)
  expect_equal(is.hier_dawid_skene(model), TRUE)

  # overall and internal shape
  expect_equal(length(model), 4)
  expect_equal(length(get_parameters(model)), 1)

  # defualt parameter values
  expect_equal(get_parameters(model)$alpha, NULL)
})

test_that("model functions error correctly", {
  expect_error(dawid_skene(beta = 4), "beta must be a square numeric matrix")
  expect_error(dawid_skene(alpha = "cat"), "alpha must be a numeric vector")
  expect_error(dawid_skene(alpha = 1:3, beta = matrix(1:4, nrow = 2)),
               "alpha and beta must have the same dimensions")
})

test_that("compute K works", {
  m <- dawid_skene(alpha = 1)
  expect_equal(get_K(m), 1)

  m <- dawid_skene(alpha = rep(1, 100))
  expect_equal(get_K(m), 100)

  m <- dawid_skene(beta = matrix(1:4, nrow = 2))
  expect_equal(get_K(m), 2)

  m <- dawid_skene(beta = matrix(1:16, nrow = 4))
  expect_equal(get_K(m), 4)
})
