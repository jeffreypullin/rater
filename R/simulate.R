#' Simulate from a rater model
#'
#' @param object a rater model object
#' @param nsim the number of simulation to perform
#' @param seed the seed for the simulation
#' @param new_params new parameters for the model
#' @param ...
#'
#' @return simulation
#' @export
#'
#' @details if new params is specifed data will be generated from the model
#'   specifed by a model is new_params parameters
#'
#' @examples
#'
# Question: is data_list the best way to refer to the object in the UI?
simulate.fit <- function(object,
                         nsim = 1,
                         seed = NULL,
                         new_params = NULL,
                         new_data_list = NULL,
                         ...) {

  model_type <- class(object)[[1]]
  out <- switch(model_type,
      "dawid_skene" = simulate_ds(object, ...),
      "hier_dawid_skene" = simulate_hierds(object, ...),
      "multinomial" = simulation_multi(object, ...))

  out
}

#' Internal funtion to simulate the dawid and skene model
#'
#' @param object a model object
#' @param ... other stuff
#'
#' @return simulated data
#'
#' @details Some notes:
#'   * Currently this does not support direct passing of z
#'   * There is basically no input checking yet - due the high number of inputs
#'   * The slightly clunky interface for new model simulation is due to needing
#'     to use the stats::simulate generic
#'
simulate_ds <- function(object, ...) {

  args <- list(...)
  if (!is.null(args$seed)) {
    set.seed(args$eed)
  }

  if (!is.null(args$new_data_list)) {
    data_list <- args$new_data_list
  } else {
    data_list <- object$new_data_list
  }

  I <- data_list$I # number of samples/patients (# of measurements)
  J <- data_list$J # tests/raters (# making observations)
  K <- data_list$K # categories/condition statuses

  if (!is.null(args$new_params)) {
    pi <- args$new_params$pi # prevalance
    theta <- args$new_params$theta # rater probablistic confusion matrix
  } else {
    # get the fitted parameters from the model object
    pi <- extract_prevalance(fit)$pi_mean
    theta <- as_array(extract_raters(fit))
  }

  z <- sample(1:K, I, replace = TRUE, prob = pi)
  data <- matrix(0, nrow = I, ncol = J)

  for(j in 1:J){
    for(i in 1:I){
      data[i, j] <- sample(1:K, 1, prob = theta[j, z[i], ])
    }
  }

  # would be more efficient to simulate long data but I don't think it matters
  out <- wide_to_long(data)

  out
}

simulate_hierds <- function(object, nsim = 1, seed = NULL, ...) {
  stop("not currently implemented")
}

simulate_multi <- function(object, nsim = 1, seed = NULL, ...) {
  stop("not currently implemented")
}

# helper to covert a list of matrices into a 3D array
as_array <- function(list) {
  len <- length(list)
  # we assume the matrices are square
  size <- vapply(list, nrow, FUN.VALUE = numeric(1))[[1]]

  out <- array(0, dim = c(len, size, size))
  for (i in 1:len) {
    out[i, , ] <- list[[i]]
  }

  out
}
