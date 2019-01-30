context("coerce_data")

test_that("coerce_data output is correct", {

  # contrived small data example
  test_data <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)

  out <- wide_to_long(test_data)
  expect_equal(dim(out), c(4, 3))
  expect_equal(max(out[, 2]), 2)
  expect_equal(max(out[, 3]), 2)

})
