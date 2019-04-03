context("data types")

test_that("input errors work for wide data", {
  expect_error(wide_data("A"), "Data must be a numeric matrix")
  expect_error(wide_data(data.frame(1:2, 1:2)), "Data must be a numeric matrix")
})

test_that("input errors work for long data", {
  expect_error(long_data("A"), "Data must be a numeric matrix with three columns")
  expect_error(long_data(data.frame(1:2, 1:2, 1:2)), "Data must be a numeric matrix with three columns")
  expect_error(long_data(matrix(1:4, nrow = 2)), "Data must be a numeric matrix with three columns")
})

test_that("input errors work for multinomial data", {
  expect_error(multinomial_data("A"), "Data must be a numeric matrix with two columns")
  expect_error(multinomial_data(data.frame(1:2, 1:2)), "Data must be a numeric matrix with two columns")
  expect_error(multinomial_data(matrix(1:9, nrow = 3)), "Data must be a numeric matrix with two columns")
})

test_that("input data is parsed to stan data correctly", {
  # need to create appropriate test data
})
