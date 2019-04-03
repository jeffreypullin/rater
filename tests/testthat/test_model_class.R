context("model_class")

# TODO: why are there more backslashes?

test_that("print works for models", {

  expect_output(
    print(ds_model),
    "Bayesian Dawid and Skene Model \\n\\nPrior parameters:\\n\\nalpha: default\\nbeta: default"
  )

})

test_that("summary works for models", {

  expect_output(summary(ds_model), "Bayesian Dawid and Skene Model")

  # Problems with the () - actual value has \(
  #expect_output(summary(multi_model), "Bayesian Multinomial (Annotator pooled) Model")

  expect_output(summary(hds_model), "Bayesian Hierarchical Dawid and Skene Model")

})

# shoudl these be split into seperate tests
test_that("is.* functions work for models", {

  # rater_model
  test_model <- 2
  expect_equal(is.rater_model(test_model), FALSE)
  class(test_model) <- "rater_model"
  expect_equal(is.rater_model(test_model), TRUE)

  # model types
  test_ds <- 2
  expect_equal(is.dawid_skene(test_ds), FALSE)
  class(test_ds) <- "dawid_skene"
  expect_equal(is.dawid_skene(test_ds), TRUE)

  test_multi <- 2
  expect_equal(is.multinomial(test_multi), FALSE)
  class(test_multi) <- "multinomial"
  expect_equal(is.multinomial(test_multi), TRUE)

  test_hds <- 2
  expect_equal(is.hier_dawid_skene(test_hds), FALSE)
  class(test_hds) <- "hier_dawid_skene"
  expect_equal(is.hier_dawid_skene(test_hds), TRUE)

})
