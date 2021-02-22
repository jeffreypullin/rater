test_that("dawid_skene() constructor works", {
  model <- dawid_skene()

  expect_s3_class(model, "rater_model")
  expect_s3_class(model, "dawid_skene")

  expect_length(model, 4)

  pars <- get_parameters(model)

  expect_length(pars, 2)
  expect_named(pars, c("alpha", "beta"))
  expect_equal(pars, list(NULL, NULL), ignore_attr = "names")
})

test_that("dawid_skene() errors correctly", {
  expect_error(
    dawid_skene(beta = "a string"),
    "beta must be a numeric matrix or array"
  )
  expect_error(
    dawid_skene(beta = list()),
    "beta must be a numeric matrix or array"
  )

  expect_error(
    dawid_skene(beta = matrix(1, nrow = 2, ncol = 3)),
    "beta must a square matrix"
  )

  expect_error(
    dawid_skene(beta = array(1, dim = rep(1, 4))),
    "`beta` must be a 3 dimensional array"
  )

  expect_error(
    dawid_skene(beta = array(1, c(3, 2, 3))),
    "Subslices of `beta` must be square matrices."
  )

  expect_error(
    dawid_skene(alpha = c(3, 3), beta = matrix(1, nrow = 3, ncol = 3)),
    "`alpha` and `beta` are not compatible.",
  )
})

test_that("Computation of K is correct in dawid_skene()", {
  expect_equal(get_K(dawid_skene(alpha = 1)), 1)
  expect_equal(get_K(dawid_skene(alpha = rep(1, 100))), 100)

  expect_equal(get_K(dawid_skene(beta = matrix(1:4, nrow = 2))), 2)
  expect_equal(get_K(dawid_skene(beta = matrix(1:16, nrow = 4))), 4)

  expect_equal(get_K(dawid_skene(beta = array(1, c(3, 2, 2)))), 2)
})

test_that("hier_dawid_skene() constructor works", {
  model <- hier_dawid_skene()

  expect_s3_class(model, "rater_model")
  expect_s3_class(model, "hier_dawid_skene")

  expect_length(model, 4)

  pars <- get_parameters(model)

  expect_named(pars, "alpha")
  expect_length(pars, 1)
  expect_equal(pars, list(NULL), ignore_attr = "names")
})

test_that("class_conditional_dawid_skene() constructor works", {
  model <- class_conditional_dawid_skene()

  expect_s3_class(model, "rater_model")
  expect_s3_class(model, "class_conditional_dawid_skene")

  expect_length(model, 4)

  pars <- get_parameters(model)

  expect_length(pars, 3)
  expect_named(pars, c("alpha", "beta_1", "beta_2"))
  expect_equal(pars, list(NULL, NULL, NULL), ignore_attr = "names")
})

test_that("class_conditional_dawid_skene() errors correctly", {
  expect_error(
    class_conditional_dawid_skene(alpha = rep(1, 2), beta_1 = rep(1, 3)),
    "Prior parameters are not compatible."
  )
})

test_that("validate alpha errors appropriately", {
  expect_error(validate_alpha("a string"))
  expect_error(validate_alpha(list()))

  expect_error(validate_alpha(c(3, 3)), NA)
  expect_error(validate_alpha(NULL), NA)
})
