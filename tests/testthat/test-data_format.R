test_that("wide_to_long error appropriately", {
  expect_error(
    wide_to_long(2),
    "`data` must be a data.frame or matrix."
  )
  expect_error(
    wide_to_long(data.frame("a")),
    "All columns in `data` must contain only numeric values."
  )
  expect_error(
    wide_to_long(data.frame(0)),
    "Some ratings are 0. All ratings must be in 1:K where K is the number of classes."
  )
})

test_that("wide_to_long converts complete data correctly", {
  wide_data <- data.frame(c(3, 2, 2), c(4, 2, 2))
  long_data <- data.frame(item = c(1, 1, 2, 2, 3, 3),
                          rater = c(1, 2, 1, 2, 1, 2),
                          rating = c(3, 4, 2, 2, 2, 2))

  expect_equal(wide_to_long(wide_data), long_data)
})

test_that("wide_to_long converts incomplete data correctly", {
  wide_data <- data.frame(c(3, 2, 2, 3, 3, NA), c(4, 2, 2, NA, NA, 4))
  long_data <- data.frame(item = c(1, 1, 2, 2, 3, 3, 4, 5, 6),
                          rater = c(1, 2, 1, 2, 1, 2, 1, 1, 2),
                          rating = c(3, 4, 2, 2, 2, 2, 3, 3, 4))

  expect_equal(wide_to_long(wide_data), long_data)
})

