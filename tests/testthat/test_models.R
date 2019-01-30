context("models")

test_that("test dawid_skene constructor", {

  model <- dawid_skene()

  # class construction
  expect_equal(is.model(model), TRUE)
  expect_equal(is.dawid_skene(model), TRUE)

  # overall and internal shape
  expect_equal(length(model), 1)
  expect_equal(length(model$parameters), 2)

  # defualt parameter values
  expect_equal(model$parameters$alpha, NULL)
  expect_equal(model$parameters$beta, NULL)

  # parameter assignment
  model <- dawid_skene(alpha = 2, beta = 4)
  expect_equal(model$parameters$alpha, 2)
  expect_equal(model$parameters$beta, 4)

})

test_that("test hier_dawid_skene constructor", {

  model <- hier_dawid_skene()

  # class construction
  expect_equal(is.model(model), TRUE)
  expect_equal(is.hier_dawid_skene(model), TRUE)

  # overall and internal shape
  expect_equal(length(model), 1)
  expect_equal(length(model$parameters), 1)

  # defualt parameter values
  expect_equal(model$parameters$alpha, NULL)

  # parameter assignment
  model <- dawid_skene(alpha = 2)
  expect_equal(model$parameters$alpha, 2)

})

test_that("test multinomial constructor", {

  model <- multinomial()

  # class construction
  expect_equal(is.model(model), TRUE)
  expect_equal(is.multinomial(model), TRUE)

  # overall and internal shape
  expect_equal(length(model), 1)
  expect_equal(length(model$parameters), 2)

  # defualt parameter values
  expect_equal(model$parameters$alpha, NULL)
  expect_equal(model$parameters$beta, NULL)

  # parameter assignment
  model <- dawid_skene(alpha = 2, beta = 4)
  expect_equal(model$parameters$alpha, 2)
  expect_equal(model$parameters$beta, 4)

})

