context("extract")

test_that("extract_raters error appropriatly", {

  expect_error(
    extract_raters(hds_fit),
    "Rater metrics cannot be extracted from the Hierachical Dawid and Skene model."
  )

  expect_warning(
    extract_raters(multi_fit, which = 5),
    "`which` arguement will be ignored as the model is of type multinomial"
  )

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

test_that("extract_prevalance output has correct form", {

  out <- extract_prevalance(ds_fit)
  prob <- out[, 2]

  expect_equal(dim(out), c(K, 3))
  expect_named(out, c("category", "prob", "sd"))

  expect_equal(sum(prob > 0), K)
  expect_equal(sum(prob), 1)

})

test_that("extract_latent_class output has correct form", {

  out <- extract_latent_class(ds_fit)

  expect_equal(dim(out), c(I, K))
  expect_equal(rowSums(out), rep(1, I))

})


test_that("extract_raters output has correct form", {

  ds_out <- extract_raters(ds_fit)
  multi_out <- extract_raters(multi_fit)

  # dawid skene

  # form
  expect_equal(is.list(ds_out), TRUE)
  expect_named(ds_out, paste("rater", rep(1:J), sep = "_"))

  # is probability
  lapply(ds_out, function(x) expect_equal(rowSums(x), rep(1, K)))

  # multinomial

  # form
  expect_equal(is.matrix(multi_out), TRUE)

  # is probability
  expect_equal(rowSums(multi_out), rep(1, K))
  expect_equal(sum(multi_out > 0), K * K)

})





