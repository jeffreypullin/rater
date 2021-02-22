test_that("print works for models with default parameters", {
  expect_snapshot(print(dawid_skene()))
  expect_snapshot(print(class_conditional_dawid_skene()))
  expect_snapshot(print(hier_dawid_skene()))
})

test_that("summary works for models", {
  expect_output(summary(dawid_skene()), "Bayesian Dawid and Skene Model")
  expect_output(
    summary(dawid_skene(alpha = c(3, 3))),
    "Bayesian Dawid and Skene Model"
  )
  expect_output(
    summary(hier_dawid_skene()),
    "Bayesian Hierarchical Dawid and Skene Model"
  )
})

test_that("is.* functions work for models", {
  expect_false(is.rater_model(2))
  expect_true(is.rater_model(dawid_skene()))

  expect_false(is.dawid_skene(2))
  expect_true(is.dawid_skene(dawid_skene()))

  expect_false(is.hier_dawid_skene(2))
  expect_true(is.hier_dawid_skene(hier_dawid_skene()))

  expect_false(is.class_conditional_dawid_skene(2))
  expect_true(is.class_conditional_dawid_skene(class_conditional_dawid_skene()))
})

test_that("get_name and get_file work", {
  expect_equal(get_name(dawid_skene()), dawid_skene()$name)
  expect_equal(get_file(dawid_skene()), dawid_skene()$file)
})
