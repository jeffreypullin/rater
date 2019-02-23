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
simulate_model <- function(model, parameters, data_list, ...) {

  model_type <- class(model)[[1]]
  out <- switch(model_type,
      "dawid_skene" = simulate_ds(model, parameters, data_list, ...),
      "hier_dawid_skene" = simulate_hierds(model, parameters, data_list, ...),
      "multinomial" = simulation_multi(model, parameters, data_list, ...))

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
simulate_ds <- function(model, parameters, data_list, ...) {

  args <- list(...)
  if (!is.null(args$seed)) {
    set.seed(args$seed)
  }

  I <- data_list$I # number of samples/patients (# of measurements)
  J <- data_list$J # tests/raters (# making observations)
  K <- data_list$K # categories/condition statuses

  pi <- parameters$pi # prevalance
  theta <- parameters$theta # rater probablistic confusion matrix

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


