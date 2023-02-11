#' Simulate data from the Dawid-Skene model
#'
#' @param pi The pi parameter of the Dawid-Skene model.
#' @param theta The theta parameter of the Dawid-Skene model.
#' @param sim_data Data to guide the simulation. The data must be in the long
#'   data format used in `rater()` except without the 'rating' column. The data
#'   specifies:
#'   * the number of items in the data, and
#'   * which raters rate each item and how many times they do so.
#' @param seed An optional random seed to use.
#'
#' @return The passed `sim_data` augmented with columns:
#'   * `"z"` containing the latent class of each item,
#'   * `"rating"` containing the simulated ratings.
#'
#' @details The number of raters implied by the entries in the rater column
#'   must match the number of raters implied by the passed theta parameter.
#'
#'   This function can also be used to simulate from the class-conditional
#'   Dawid-Skene model by specifying theta in the required form (i.e where
#'   all off-diagonal entries of the error matrices are equal.)
#'
#' @examples
#'
#' \donttest{
#'
#' J <- 5
#' K <- 4
#' pi <- rep(1 / K, K)
#' theta <- make_theta(0.7, J, K)
#' sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))
#'
#' simulations <- simulate_dawid_skene_model(pi, theta, sim_data)
#' simulations
#'
#' }
#'
#' @export
#'
simulate_dawid_skene_model <- function(pi, theta, sim_data, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check pi.
  if (!(is.numeric(pi) && isTRUE(all.equal(sum(pi), 1)))) {
    stop("`pi` must be a numeric vector that sums to 1.", call. = FALSE)
  }

  pi_K <- length(pi)

  theta_dim <- dim(theta)
  if (!length(theta_dim) == 3) {
    stop("`theta` must be a three-dimensional array.", call. = FALSE)
  }

  # Check theta.
  if (length(unique(theta_dim[2:3])) != 1) {
    stop("The last two dimensions of `theta` must be the same.", call. = FALSE)
  }

  theta_J <- theta_dim[[1]]
  theta_K <- theta_dim[[2]]

  for (j in seq_len(theta_J)) {
    for (k in seq_len(theta_K)) {
      if (!isTRUE(all.equal(sum(theta[j, k, ]), 1))) {
        stop("theta[", j, ", ", k, ", ] must sum to 1.", call. = FALSE)
      }
    }
  }

  # Check consistency of pi and theta.
  if (pi_K != theta_K) {
    stop("The number of ratings implied by pi and theta is not the same.",
         call. = FALSE)
  }

  # Check the simulation data.
  sim_data <- as.data.frame(sim_data)
  col_names <- colnames(sim_data)
  if (ncol(sim_data) != 2 || !all(c("item", "rater") %in% col_names)) {
    stop("`sim_data` must have two columns 'item' and 'rater'", call. = FALSE)
  }

  if (!all(sim_data$rater %in% seq_len(theta_J))) {
    stop("The number of raters implied by theta and implied by the simulation ",
         "data must match.", call. = FALSE)
  }

  # Perform the simulation.
  n <- nrow(sim_data)
  K <- pi_K
  I <- max(sim_data$item)

  item_z <- sample(1:K, size = I, replace = TRUE, prob = pi)
  z <- item_z[sim_data$item]

  ratings <- numeric(n)
  for (i in seq_len(n)) {
    j <- sim_data$rater[[i]]
    ratings[[i]] <- sample(1:K, 1, prob = theta[j, z[[i]], ])
  }

  sim <- cbind(sim_data, z = z, ratings = ratings)
  sim
}

#' Simulate data from the hierarchical Dawid-Skene model
#'
#' @param pi The pi parameter of the hierarchical Dawid-Skene model.
#' @param mu The mu parameter of the hierarchical Dawid-Skene model.
#' @param sigma The sigma parameter of the hierarchical Dawid-Skene model.
#' @param sim_data Data to guide the simulation. The data must be in the long
#'   data format used in `rater()` except without the 'rating' column. The data
#'   specifies:
#'   * the number of items in the data, and
#'   * which raters rate each item and how many times they do so.
#' @param seed An optional random seed to use.
#'
#' @return The passed `sim_data` augmented with columns:
#'   * `"z"` containing the latent class of each item,
#'   * `"rating"` containing the simulated rating.
#'
#' @details The number of raters implied by the entries in the rater column
#'   must match the number of raters implied by the passed theta parameter.
#'
#' @examples
#'
#' \donttest{
#'
#' J <- 5
#' K <- 4
#'
#' pi <- rep(1 / K, K)
#'
#' mu <- matrix(0, nrow = K, ncol = K)
#' diag(mu) <- 5
#'
#' sigma <- matrix(sqrt(2) / sqrt(pi), nrow = K, ncol = K)
#'
#' sim_data <- data.frame(item = rep(1:2, each = 5), rater = rep(1:5, 2))
#'
#' sim_result <- simulate_hier_dawid_skene_model(pi, mu, sigma, sim_data)
#'
#' sim_result$sim
#' sim_result$theta
#'
#' }
#'
#' @importFrom stats rnorm
#'
#' @export
#'
simulate_hier_dawid_skene_model <- function(pi, mu, sigma, sim_data, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check pi.
  if (!(is.numeric(pi) && isTRUE(all.equal(sum(pi), 1)))) {
    stop("`pi` must be a numeric vector that sums to 1.", call. = FALSE)
  }

  pi_K <- length(pi)

  # Check mu.
  if (!(is.matrix(mu) && length(unique(dim(mu))) == 1)) {
    stop("`mu` must be a square matrix.")
  }

  mu_K <- nrow(mu)

  # Check sigma.
  if (!(is.matrix(sigma) && length(unique(dim(sigma))) == 1 && all(sigma > 0))) {
    stop("`sigma` must be a square matrix with all elements be greater then 0.")
  }

  sigma_K <- nrow(sigma)

  # Check consistency of parameters.
  if (length(unique(c(pi_K, mu_K, sigma_K))) != 1) {
    stop("`pi`, `mu` and `sigma` imply different numbers of categories.",
         call. = FALSE)
  }

  # Check the simulation data.
  sim_data <- as.data.frame(sim_data)
  col_names <- colnames(sim_data)
  if (ncol(sim_data) != 2 || !all(c("item", "rater") %in% col_names)) {
    stop("`sim_data` must have two columns 'item' and 'rater'", call. = FALSE)
  }

  # Perform the simulation.
  n <- nrow(sim_data)
  K <- pi_K
  J <- max(sim_data$rater)
  I <- max(sim_data$item)

  item_z <- sample(1:K, size = I, replace = TRUE, prob = pi)
  z <- item_z[sim_data$item]

  gamma <- array(0, c(J, K, K))
  for (j in seq_len(J)) {
    for (k in seq_len(K)) {
      for (i in seq_len(K)) {
        gamma[j, k, k] <- stats::rnorm(1, mu[k, k], sigma[k, k])
      }
    }
  }

  theta <- array(0, c(J, K, K))
  for (j in seq_len(J)) {
    for (k in seq_len(K)) {
      theta[j, k, ] <- softmax(gamma[j, k, ])
    }
  }

  ratings <- numeric(n)
  for (i in seq_len(n)) {
    j <- sim_data$rater[[i]]
    ratings[[i]] <- sample(1:K, 1, prob = theta[j, z[[i]], ])
  }

  sim <- cbind(sim_data, z = z, ratings = ratings)

  out <- list(theta = theta, sim = sim)
  out
}

#' Make a theta parameter
#'
#' @param diag_values The diagonal entries of each error matrix.
#' @param J The number of raters (The umber matrices in 3D array).
#' @param K The number of latent classes.
#'
#' @return A c(J, K, K) array; the theta parameter
#'
#' @details The `diag_values` argument can either be a numeric vector of length
#'   1 or J. If it is length J, the jth element is the diagonal values of the
#'   error matrix for the jth rater. If it is length 1 all raters have the same
#'   diagonal values.
#'
#' @examples
#'
#' theta <- make_theta(0.7, 5, 4)
#' theta[1, , ]
#'
#' @export
#'
make_theta <- function(diag_values, J, K) {

  if (!(is.numeric(diag_values) && all(diag_values > 0 & diag_values < 1))) {
    stop("`diag_values` must be a probability.", call. = FALSE)
  }

  if (length(diag_values) != 1 & length(diag_values) != J) {
    stop("`diag_values` must be length 1 or length `J`.", call. = FALSE)
  }

  if (length(diag_values) == 1) {
    diag_values <- rep(diag_values, J)
  }

  theta <- array(0, dim = c(J, K, K))
  for (j in 1:J) {
    theta[j, ,] <- array((1 - diag_values[[j]]) / (K - 1))
    for (k in 1:K) {
      theta[j, k, k] <- diag_values[[j]]
    }
  }

  theta
}

#' Produce simulation data from a 'complete' rating design
#'
#' @param I The number of items.
#' @param J The number of raters.
#' @param N The number of times each rater rates each item.
#'
#' @return Simulation data in the format required by
#'   [simulate_dawid_skene_model()] or [simulate_hier_dawid_skene_model()].
#'
#' @details A 'complete' rating design is situation where every rater rates
#'   each item the same number of times. In this function the number of times
#'   each rater rates each item is `N`.
#'
#' @examples
#'
#' make_complete_rating_design_sim_data(100, 5, 2)
#'
#' @export
#'
make_complete_rating_design_sim_data <- function(I, J, N) {
  data.frame(item = rep(1:I, each = J * N), rater = rep(1:J, I * N))
}

