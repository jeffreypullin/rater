#' Fit noisy catergrical rating models using Stan
#'
#' @param data Data for the model in long format as a three column dataframe of
#'   ii, jj, yy
#' @param model Model to fit to data of class model
#' @param method method the method used to fit the model
#' @param inits the initialization points of the fitting algorithm
#' @param ... extra parameters to be passed to the Stan fitting interface
#'
#' @return An object of type fit containing the fitted parameters
#'
#' @importFrom rstan sampling optimizing
#'
#' @export
#'
rater <- function(data, model, method = "mcmc", inits = NULL, ...) {
  method <- match.arg(method, choices = c("mcmc", "optim"))
  validate_input(data, model)
  stan_data_list <- get_stan_data(data)

  # check the priors and data are consistent
  check_K(stan_data_list, model)

  # create the full passed info for stan and the inits
  stan_data <- c(stan_data_list, parse_priors(model, stan_data_list$K))

  if (is.null(inits)) {
    inits <- creat_inits(model, stan_data_list)
  }

  # TODO: this could be made more complex if automatic switching is used
  file <- get_stan_file(data, model)

  if (method == "mcmc") {
    draws <- rstan::sampling(stanmodels[[file]], stan_data, init = inits, ...)
    out <- new_mcmc_fit(model = model, draws = draws, data = data)
  } else if (method == "optim") {
    estimates <- rstan::optimizing(stanmodels[[file]], stan_data, init = inits, ...)
    out <- new_optim_fit(model = model, estimates = estimates, data = data)
  }

  out
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
#' @param stan_data data in stan format
#' @param model the passed model
#'
#' @return the fully reliased prior parameters
#'
check_K <- function(stan_data, model) {
  # NB: this does not/cannot tell the user which of the pars is inconsistent
  # but we can return a vector (with NULLs and parse cleverly)
  if (!is.null(model$K) && (stan_data$K != model$K)) {
    stop("The number of categories is inconsistent between data and the prior",
         "parameters", call. = FALSE)
  }
}

#' Helper get the correct stan file to run for model/data combination
#'
#' @param data a rater_data object
#' @param model a rater_model object
#'
#' @return the name (no .stan) of the stan file that should be run
#'
get_stan_file <- function(data, model) {
  # we assume here that only legal inputs are considered
  if (is.table_data(data)) {
    file <- "table_data"
  } else if (is.homo_dawid_skene(model)) {
    file <- "dawid_skene"
  } else {
    file <- get_file(model)
  }
  file
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
  if (is.table_data(data) & !is.dawid_skene(model)) {
    stop("table data can only be uses with the Dawid and Skene model", call. = FALSE)
  }
  if (is.homo_dawid_skene(model) & (get_stan_data(data)$J != 1)) {
    stop("The homogenous dawid and skene model must be fit with 1 rater", call. = FALSE)
  }
}

#' Creates inits for the stan MCMC chains
#'
#' @param model rater model
#' @param stan_data data in list form
#'
#' @return intits in the format required by stan
#'
creat_inits <- function(model, stan_data) {
  # better to have another short unique id...
  switch(get_file(model),
    "dawid_skene" = dawid_skene_inits(stan_data$K, stan_data$J),
    "hierarchical_dawid_skene" = "random",
    "homogenous_dawid_skene" = dawid_skene_inits(stan_data$K, 1),
    stop("Unsupported model type", call. = FALSE))
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
  for (j in seq_len(J)) {
      diag(theta_init[j, ,]) <- 0.8
  }
  function(n) list(theta = theta_init, pi = pi_init)
}
