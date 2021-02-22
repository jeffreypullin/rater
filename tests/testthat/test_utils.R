test_that("logsumexp works", {
  expect_equal(logsumexp(c(log(1))), 0)
  expect_equal(logsumexp(1), 1)
})

test_that("softmax works", {
  expect_equal(softmax(c(1, 1)), c(1 / 2, 1 / 2))
})
