test_that("`simulate_dawid_skene_model()` errors appropriately", {

  J <- 5
  K <- 4
  pi <- rep(1 / K, K)
  theta <- make_theta(0.7, J, K)
  sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))

  expect_error(
    simulate_dawid_skene_model("a", theta, sim_data),
    "`pi` must be a numeric vector that sums to 1."
  )

  expect_error(
    simulate_dawid_skene_model(c(1, 1, 1), theta, sim_data),
    "`pi` must be a numeric vector that sums to 1."
  )

  expect_error(
    simulate_dawid_skene_model(pi, matrix(0, nrow = 2, ncol = 2), sim_data),
    "`theta` must be a three-dimensional array."
  )

  expect_error(
    simulate_dawid_skene_model(pi, array(0, dim = c(5, 4, 5)), sim_data),
    "The last two dimensions of `theta` must be the same."
  )

  bad_theta <- theta
  bad_theta[1, 1, 1] <- 2

  expect_error(
    simulate_dawid_skene_model(pi, bad_theta, sim_data),
  )

  expect_error(
    simulate_dawid_skene_model(rep(1 / 6, 6), theta, sim_data),
    "The number of ratings implied by pi and theta is not the same."
  )

  expect_error(
    simulate_dawid_skene_model(pi, theta, data.frame(a = 1, b = 2, c = 3)),
    "`sim_data` must have two columns 'item' and 'rater'"
  )

  expect_error(
    simulate_dawid_skene_model(pi, theta, data.frame(item = 1, raterr = 1)),
    "`sim_data` must have two columns 'item' and 'rater'"
  )

  expect_error(
    simulate_dawid_skene_model(pi, theta, data.frame(item = 1, rater = 6)),
    "The number of raters implied by theta and implied by the simulation data must match."
  )
})

test_that("`simulate_dawid_skene_model()` has sensible output", {

  J <- 5
  K <- 4
  pi <- rep(1 / K, K)
  theta <- make_theta(0.7, J, K)
  sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))
  sim <- simulate_dawid_skene_model(pi, theta, sim_data)

  expect_lte(max(sim$ratings), length(pi))
  expect_gte(min(sim$ratings), 1)

  expect_lte(max(sim$z), length(pi))
  expect_gte(min(sim$z), 1)

  expect_equal(length(unique(sim[sim$item == 1, "z"])), 1)
  expect_equal(length(unique(sim[sim$item == 2, "z"])), 1)

  expect_equal(sim[, c("item", "rater")], sim_data)
})

test_that("`simulate_hier_dawid_skene_model()` errors appropriately", {

  J <- 5
  K <- 4
  pi <- rep(1 / K, K)
  mu <- matrix(0, nrow = K, ncol = K)
  diag(mu) <- 5
  sigma <- matrix(sqrt(2) / sqrt(pi), nrow = K, ncol = K)
  sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))

  expect_error(
    simulate_hier_dawid_skene_model("a", mu, sigma, sim_data),
    "`pi` must be a numeric vector that sums to 1."
  )

  expect_error(
    simulate_hier_dawid_skene_model(c(1, 1, 1), mu, sigma, sim_data),
    "`pi` must be a numeric vector that sums to 1."
  )

  expect_error(
    simulate_hier_dawid_skene_model(pi, c(1, 1, 1), sigma, sim_data),
    "`mu` must be a square matrix."
  )

  expect_error(
    simulate_hier_dawid_skene_model(pi, mu, c(1, 1, 1), sim_data),
    "`sigma` must be a square matrix with all elements be greater then 0."
  )

  expect_error(
    simulate_hier_dawid_skene_model(rep(1 / 5, 5), mu, sigma, sim_data),
    "`pi`, `mu` and `sigma` imply different numbers of categories."
  )

  expect_error(
    simulate_hier_dawid_skene_model(pi, mu, sigma, data.frame(a = 1, b = 2, c = 3)),
    "`sim_data` must have two columns 'item' and 'rater'"
  )

  expect_error(
    simulate_hier_dawid_skene_model(pi, mu, sigma, data.frame(item = 1, raterr = 1)),
    "`sim_data` must have two columns 'item' and 'rater'"
  )
})

test_that("`simulate_hier_dawid_skene_model()` has sensible output", {

  J <- 5
  K <- 4
  pi <- rep(1 / K, K)
  mu <- matrix(0, nrow = K, ncol = K)
  diag(mu) <- 5
  sigma <- matrix(sqrt(2) / sqrt(pi), nrow = K, ncol = K)
  sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))
  sim_out <- simulate_hier_dawid_skene_model(pi, mu, sigma, sim_data)
  sim <- sim_out$sim
  theta <- sim_out$theta

  expect_lte(max(sim$ratings), length(pi))
  expect_gte(min(sim$ratings), 1)

  expect_lte(max(sim$z), length(pi))
  expect_gte(min(sim$z), 1)

  expect_equal(length(unique(sim[sim$item == 1, "z"])), 1)
  expect_equal(length(unique(sim[sim$item == 2, "z"])), 1)

  expect_equal(sim[, c("item", "rater")], sim_data)

  expect_equal(dim(theta), c(J, K, K))
  expect_equal(sum(theta[1, 1, ]), 1)
})

test_that("`make_theta` works", {

  J <- 5
  K <- 4

  expect_error(
    make_theta("a", J, K),
    "`diag_values` must be a probability."
  )
  expect_error(
    make_theta(2, J, K),
    "`diag_values` must be a probability."
  )

  expect_error(
    make_theta(rep(0.7, 6), J, K),
    "`diag_values` must be length 1 or length `J`."
  )

  expect_equal(make_theta(0.7, J, K), make_theta(rep(0.7, J), J, K))

  theta <- make_theta(0.7, J, K)
  expect_equal(theta[1, 1, 1], 0.7)
  expect_equal(dim(theta), c(J, K, K))
  expect_equal(theta[1, 1, 2], (1 - 0.7) / (K - 1))
})

test_that("`make_complete_rating_design_sim_data()`", {

  I <- 10
  J <- 5
  N <- 2
  sim_data <- make_complete_rating_design_sim_data(I, J, N)

  expect_equal(colnames(sim_data), c("item", "rater"))
  expect_equal(ncol(sim_data), 2)
  expect_s3_class(sim_data, "data.frame")

  expect_equal(nrow(sim_data), I * J * N)
  expect_equal(max(sim_data$item), I)
  expect_equal(max(sim_data$rater), J)
})
