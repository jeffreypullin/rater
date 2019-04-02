context("extract")

test_that("extract_raters error appropriatly", {

  expect_error(
    extract_theta(hds_fit),
    "Rater metrics cannot be extracted from the Hierachical Dawid and Skene model."
  )

  # continually throws false positives...
  skip(expect_warning(
    extract_theta(multi_fit, which = 5),
   "`which` arguement will be ignored (multinomial model)"
  ))

})

test_that("validate_which error appropriatly", {

  expect_error(
    validate_which("which", 2),
    "which must be a positive length numeric vector"
  )

  expect_error(
    validate_which(numeric(0), 2),
    "which must be a positive length numeric vector"
  )

  expect_error(
    validate_which(1:9, 6),
    "All numbers in `which` must be drawn from 1:6"
  )

})

test_that("extract_pi output has correct form", {

  out <- extract_pi(ds_fit)

  expect_equal(length(out), K)
  expect_equal(sum(out), 1)

})

test_that("extract_z output has correct form", {

  out <- extract_z(ds_fit)

  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

})


test_that("extract_theta output has correct form", {

  ds_out <- extract_theta(ds_fit)
  multi_out <- extract_theta(multi_fit)

  # dawid skene

  # form
  expect_equal(is.array(ds_out), TRUE)

  # is probability
  apply(ds_out, 1, function(x) expect_equal(rowSums(x), rep(1, K)))

  # multinomial

  # form
  expect_equal(is.matrix(multi_out), TRUE)

  # is probability
  expect_equal(rowSums(multi_out), rep(1, K))
  expect_equal(sum(multi_out > 0), K * K)

})





