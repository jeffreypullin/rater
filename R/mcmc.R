#' Fit noisy catergrical rating models using HMC via Stan
#'
#' @param data Data for the model in long format as a three column dataframe of
#'   ii, jj, yy
#' @param model Model to fit to data of class model
#' @param control List of extra parameters, iter, adapt_delta, etc. to be
#' passed to the Stan fitting interface
#'
#' @return An object of type fit containing the fitted parameters
#' @export
mcmc <- function(data, model, ...) {

  validate_data(data)

  data_list <- parse_data(data)

  prior_list <- parse_priors(model, data_list)

  stan_data <- c(data_list, prior_list)

  draws <- rstan::sampling(stanmodels[[get_file(model)]], stan_data, ...)

  check_convergence(draws)

  fit <- list(model = model, draws = draws)
  class(fit) <- "fit"

  fit

}


validate_data <- function(data) {

  if (ncol(data) != 3) {
    stop("Data must be in 'long' format", call. = FALSE)
  }

  inds <- numeric(0)
  for (i in 1:3){
    if (!is.numeric(data[, i])) {
      inds <- c(inds, i)
    }
  }

  if (length(inds) > 0) {
    stop("Columns", paste(inds, collapse = ", "), "are not numeric",
    call. = FALSE)
  }

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

  if (class(model)[[1]] == "dawid_skene") {
     priors <- parse_priors_ds(model, data_list)
  } else if (class(model)[[1]] == "hier_dawid_skene"){
     priors <- parse_priors_hierds(model, data_list)
  } else {
    stop("Model type not supported", call. = FALSE)
  }

  priors

}

parse_priors_ds <- function(model, data_list) {
  out <- model$parameters
  K <- data_list$K

  # Use default specification for alpha
  # small code duplcation in check - how to abstract
  if (is.null(out$alpha)) {
    out$alpha <- default_alpha(K)
  }

  # Use default specification for beta
  if (is.null(out$beta)) {
    beta_default <- matrix(1, nrow = K, ncol = K)
    diag(beta_default) <- 2.5 * K

    out$beta <-  beta_default
  }

  validate_alpha(out$alpha, K)

  # valdiate beta parameter
  if (!all(dim(params$beta) == rep(K, 2))) {
    stop("Beta must be of dimension", K, "x", K, ".", call. = FALSE)
  }

  out

}

parse_priors_hierds <- function(model, data_list) {
  out <- model$parameters
  K <- data_list$K

  # Use default specification for alpha
  if (is.null(out$alpha)) {
    out$alpha <- default_alpha(K)
  }

  validate_alpha(out$alpha, K)

  out

}


validate_alpha <- function(alpha, K) {

  if (length(alpha) != K) {
    stop("Alpha must of length", K, "the number of categories in the data",
         call. = FALSE)
  }

}

default_alpha <- function(K){
  rep(3, K)
}
