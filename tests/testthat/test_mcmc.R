context("mcmc")

# TODO Clean this up - its a bit ugly

test_that("mcmc returns correctly", {

  expect_equal(is.fit(ds_fit), TRUE)

})

test_that("parse priors are correct", {

  # dawid skene and multinomial model

  anesthesia_list <- parse_data(dawid_skene(), anesthesia)

  # test default priors
  ds_priors <- parse_priors_ds(dawid_skene(), anesthesia_list)

  # construct the default priors
  default_alpha <- rep(3, K)
  default_beta <- matrix(1, nrow = K, ncol = K)
  diag(default_beta) <- 2.5 * K

  expect_equal(ds_priors$alpha, default_alpha)
  expect_equal(ds_priors$beta, default_beta)

  # test non-default priors
  test_alpha <- rep(9, K)
  test_beta <- matrix(17, nrow = K, ncol = K)
  ds_priors <- parse_priors_ds(dawid_skene(alpha = test_alpha, beta = test_beta), anesthesia_list)

  expect_equal(ds_priors$alpha, test_alpha)
  expect_equal(ds_priors$beta, test_beta)

  # hierarchical model

  pooled_anesthesia_list <- parse_data(multinomial(), pooled_anesthesia)

  # default priors
  hds_priors <- parse_priors_hierds(hier_dawid_skene(), pooled_anesthesia_list)

  expect_equal(hds_priors$alpha, default_alpha)
  # no theta prior passed

  # non-default priors
  hds_priors <- parse_priors_hierds(hier_dawid_skene(alpha = test_alpha), pooled_anesthesia_list)

  expect_equal(hds_priors$alpha, test_alpha)

})

test_that("parse_priors dispatches correctly", {

  anesthesia_list <- parse_data(dawid_skene(), anesthesia)
  pooled_anesthesia_list <- parse_data(multinomial(), pooled_anesthesia)

  expect_equal(
    parse_priors(dawid_skene(), anesthesia_list),
    parse_priors_ds(dawid_skene(), anesthesia_list)
  )

  expect_equal(
    parse_priors(multinomial(), pooled_anesthesia_list),
    parse_priors_ds(multinomial(), pooled_anesthesia_list)
  )

  expect_equal(
    parse_priors(hier_dawid_skene(), anesthesia_list),
    parse_priors_hierds(hier_dawid_skene(), anesthesia_list)
  )

})

test_that("parse_priors errors correctly", {

  anesthesia_list <- parse_data(dawid_skene(), anesthesia)
  pooled_anesthesia_list <- parse_data(multinomial(), pooled_anesthesia)

  expect_error(
    parse_priors_ds(dawid_skene(alpha = rep(9, 9)), anesthesia_list),
    "Alpha must of length 4, the number of categories in the data"
  )

  expect_error(
    parse_priors_ds(dawid_skene(beta = matrix(1, ncol = 9)), anesthesia_list),
    "Beta must be of dimension 4 x 4"
  )

  expect_error(
    parse_priors_hierds(dawid_skene(alpha = rep(9, 9)), anesthesia_list),
    "Alpha must of length 4, the number of categories in the data"
  )

})


test_that("parse data dispatches correctly", {

  expect_equal(parse_data(dawid_skene(), anesthesia), parse_data_raters(anesthesia))
  expect_equal(parse_data(multinomial(), anesthesia), parse_data_noraters(anesthesia))

})

test_that("parse data functions output has correct form", {

  ds_parsed <- parse_data(dawid_skene(), anesthesia)
  expect_length(ds_parsed, 7)
  expect_named(ds_parsed, c("ii", "jj", "y", "I", "J", "K", "N"))

  multi_parsed <- parse_data(multinomial(), pooled_anesthesia)
  expect_length(multi_parsed, 5)
  expect_named(multi_parsed, c("ii", "y", "I", "K", "N"))

})

test_that("validate data errors correctly", {

  expect_error(
    validate_data(multinomial(), anesthesia),
    "For the multinomial model data must be in the following format:\nColumn 1: Item index\nColumn 2: Annotation - rating given\n"
  )

  expect_error(
    validate_data(dawid_skene(), pooled_anesthesia),
    "For the partially pooled or unpooled models data must be in the following format:\nColumn 1: Item index\nColumn 2: Annotator - rater\nColumn 2: Annotation - rating given\n"
  )

  test <- matrix(1:4, ncol = 4)

  expect_error(
    validate_data(dawid_skene(), test),
    "Data must be in 'long' format"
  )

  test <- matrix(c("1", "1", "1"), ncol = 3)

  expect_error(
    validate_data(dawid_skene(), test),
    "Columns 1, 2, 3 are not numeric"
  )

})

test_that("alpha helpers work", {

  n <- 9
  expect_equal(default_alpha(n), rep(3, n))

  expect_error(validate_alpha(c(2,3,4), 4), "Alpha must of length 4, the number of categories in the data")

})

# TODO Test creat_inits
