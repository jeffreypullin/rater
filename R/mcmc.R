#' Fit noisy catergrical rating models using HMC via Stan
#'
#' @param data Data for the model in long format as a three column dataframe of
#'   ii, jj, yy
#' @param model Model to fit to data of class model
#' @param control List of extra parameters, iter, adapt_delta, etc. to be
#' passed to the Stan fitting interface
#'
#' @return An object of type fit containing the fitted parameters
#'
#' @examples
#'
#'

mcmc <- function(data, model, control = NULL) {

  validate_data(data)

  data_list <- parse_data(data)

  prior_list <- parse_priors(model, data_list)

  stan_data <- c(data_list, prior_list)

  stan_model <- rstan::stan_model(get_file(model))

  draws <- rstan::sampling(stan_model, stan_data)

  draws

}

# Helpers

validate_data <- function(data) {

  if (ncol(data) != 3) {
    stop("Data must be in 'long' format", call. = FALSE)
  }

  # Should check data is numeric

}

parse_data <- function(data) {

  ii <- data[, 1] # item index for each annotation
  jj <- data[, 2] # rater index for each annotation
  y  <- data[, 3] # annotation
  I  <- max(ii)   # number of items
  J  <- max(jj)   # number of raters
  K  <- max(y)    # number of categories
  N <- nrow(data) # total number of annotations

  out <- list(ii = ii, jj = jj, y = y, I = I, J = J, K = K, N = N)

  out

}

parse_priors <- function(model, data_list) {

  # This is *currently* only intended for defualt dawid-skene
  # Defualt priors in the function come from the Stan Manual
  out <- model$parameters
  K <- data_list$K

  if (is.null(out$alpha)) {
    # Use default specification for alpha
    out$alpha <- rep(3, K)
  }

  if (is.null(out$beta)) {
    # Use default specification for beta
    beta_default <- matrix(1, nrow = K, ncol = K)
    diag(beta_default) <- 2.5 * K

    out$beta <-  beta_defualt
  }

  out

}
