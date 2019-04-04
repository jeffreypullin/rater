#' Fit noisy catergrical rating models using HMC via Stan
#'
#' @param data Data for the model in long format as a three column dataframe of
#'   ii, jj, yy
#' @param model Model to fit to data of class model
#' @param ... extra parameters to be passed to the Stan fitting interface
#'
#' @return An object of type fit containing the fitted parameters
#' @export
#'
mcmc <- function(data, model, ...) {
  validate_input(data, model)
  stan_data_list <- get_stan_data(data)

  # check the priors and data are consistent
  check_K(stan_data_list, model)

  # create the full passed info for stan and the inits
  stan_data <- c(stan_data_list, parse_priors(model, stan_data_list$K))
  inits <- creat_inits(model, stan_data_list)

  # sample the model
  draws <- rstan::sampling(stanmodels[[get_file(model)]],
                           stan_data, init = inits, ...)

  new_mcmc_fit(model = model, draws = draws, data = data)
}

#' Helper to check if passed data and model are valid and consistent
#'
#' @param data rater_data
#' @param model rater_model
#'
validate_input <- function(data, model) {
  if (!is.rater_data(data)) {
    stop("data must be a rater data type", call. = FALSE)
  }
  if (!is.rater_model(model)) {
    stop("model must be a rater model", call. = FALSE)
  }
}

#' Converts default prior parameter specification to full priors
#'
#' @param model the rater_model
#' @param K the number of categories
#'
#' @return the fully reliased prior parameters
#'
parse_priors <- function(model, K) {
  pars <- get_parameters(model)
  if (is.null(pars$alpha)) {
    pars$alpha <- rep(3, K)
  }
  if (is.null(pars$beta)) {
    pars$beta <- matrix(1, nrow = K, ncol = K)
    diag(pars$beta) <- 2.5 * K
  }
  pars
}

#' Helper to check if the prior parameters and data have consistent dimensions
#'
#' @param model the rater_model
#' @param K the number of categories
#'
#' @return the fully reliased prior parameters
#'
check_K <- function(stan_data, model) {
  # NB: this doesnot/cannot tell the user which of the pars is inconsistent
  # but we can return a vector (with NULLs and parse cleverly)
  if (!is.null(model$K) && (stan_data$K != model$K)) {
    stop("The number of categories is inconsistent between data and the prior",
         "parameters", call. = FALSE)
  }
}

#' Creates inits for the stan MCMC chains
#'
#' @param model rater model
#' @param data_list data in list form
#'
#' @return intits in the format required by stan
#'
creat_inits <- function(model, stan_data) {
  # better to have another short unique id...
  switch(get_file(model),
    "multinomial" = multinomial_inits(stan_data$K, stan_data$J),
    "dawid_skene" = dawid_skene_inits(stan_data$K, stan_data$J),
    "hierarchical_dawid_skene" = "random",
    stop("Unsupported model type", call. = FALSE))
}

#' Creates inits for the multinomial model
#'
#' @param K number of categories
#' @param J number of raters
#'
#' @return inits in the format required by stan
#'
multinomial_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- array(0.2 / (K - 1), c(K, K))
  diag(theta_init) <- 0.8
  function(n) list(theta = theta_init, pi = pi_init)
}

#' Creates inits for the dawid and skene model
#'
#' @param K number of categories
#' @param J number of raters
#'
#' @return inits in the format required by stan
#'
dawid_skene_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- array(0.2 / (K - 1), c(J, K, K))
  for (j in 1:J) {
      diag(theta_init[j, ,]) <- 0.8
  }
  function(n) list(theta = theta_init, pi = pi_init)
}
