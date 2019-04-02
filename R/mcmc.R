#' Fit noisy catergrical rating models using HMC via Stan
#'
#' @param data Data for the model in long format as a three column dataframe of
#'   ii, jj, yy
#' @param model Model to fit to data of class model
#' @param ... extra parameters to be passed to the Stan fitting interface
#'
#' @return An object of type fit containing the fitted parameters
#' @export
# mcmc <- function(data, model, ...) {
#
#   validate_data(model, data)
#
#   data_list <- parse_data(model, data)
#
#   prior_list <- parse_priors(model, data_list)
#
#   stan_data <- c(data_list, prior_list)
#
#   inits <- creat_inits(model, data_list)
#
#   draws <- rstan::sampling(stanmodels[[get_file(model)]],
#                            stan_data,
#                            init = inits,
#                            ...)
#
#   check_convergence(draws)
#
#   fit <- fit(model = model, draws = draws, data = data)
#
#   fit
#
# }

mcmc <- function(data, model, ...) {
  stan_data_list <- get_stan_data(data)

  # check the priors and data are consistent
  check_K(stan_data_list, model)

  # create the full passed info for stan and the inits
  stan_data <- c(stan_data_list, parse_priors(model))
  inits <- creat_inits(model, stan_data_list)

  # sample the model
  draws <- rstan::sampling(stanmodels[[get_file(model)]],
                           stan_data, init = inits, ...)

  new_mcmc_fit(model = model, draws = draws, data = data)
}

# K...
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

# not sure about name
check_K <- function(stan_data, model) {
  # stan_data$K must be > 0
  # NB: this doesnot/cannot tell the user which of the pars is inconsistent
  if (stan_data$K != model$K) {
    stop("The number of categories is inconsistent between data and the prior",
         "parameters", call. = FALSE)
  }
}


#'
#' #' Convert passed data into Stan data format for a model wit raters
#' #'
#' #' @param data data in 'long format'
#' #' @return Data in format required by Stan
#' parse_data_raters <- function(data) {
#'
#'   ii <- data[, 1] # item index for each annotation
#'   jj <- data[, 2] # rater index for each annotation
#'   y  <- data[, 3] # annotation
#'   I  <- max(ii)   # number of items
#'   J  <- max(jj)   # number of raters
#'   K  <- max(y)    # number of categories
#'   N <- nrow(data) # total number of annotations
#'
#'   out <- list(ii = ii, jj = jj, y = y, I = I, J = J, K = K, N = N)
#'
#'   out
#'
#' }

#' #' Convert passed data into Stan data format for a model without raters
#' #'
#' #' @param data data in 'long format'
#' #' @return Data in format required by Stan
#' parse_data_noraters <- function(data) {
#'
#'   ii <- data[, 1]  # item index for each annotation
#'   y  <- data[, 2]  # annotation
#'   I  <- max(ii)    # number of items
#'   K  <- max(y)     # number of categories
#'   N  <- nrow(data) # total number of annotations
#'
#'   out <- list(ii = ii, y = y, I = I, K = K, N = N)
#'
#'   out
#'
#' }

#' Convert passed data into Stan data format
#'
#'
#' @param model model object
#' @param data data in 'long format'
#'
#' @return Data in format required by Stan
# parse_data <- function(model, data) {
#
#   if (is.multinomial(model)) {
#     data_list <- parse_data_noraters(data)
#   } else {
#     data_list <- parse_data_raters(data)
#   }
#
#   data_list
#
# }

#' Converts the passed priors into the appropriate format for Stan
#'
#' In paticular this function creates default priors if no priors are passed
#' and checks if passed priors are of the correct dimension.
#'
#' @param model the model passed to mcmc
#' @param data_list data already parsed into list format
#'
#' @details This function needs to see the passed data to check that the priors
#' are the correct dimension. It needs to see the model type because diffrent
#' models require different priors. This function dispatches into two sub
#' functions depending on which model is passed
# parse_priors <- function(model, data_list) {
#   if (is.dawid_skene(model) || is.multinomial(model)) {
#      priors <- parse_priors_ds(model, data_list)
#   } else if (is.hier_dawid_skene(model)){
#      priors <- parse_priors_hierds(model, data_list)
#   } else {
#     stop("Model type not supported", call. = FALSE)
#   }
#
#   priors
#
# }

#' #' Converts the passed priors for the Dawid and Skene model
#' #'
#' #' See parse_priors for details
#' #'
#' #' @param model rater model
#' #' @param data_list data converted to Stan list format see \code{parse_data}
#' parse_priors_ds <- function(model, data_list) {
#'   out <- model$parameters
#'   K <- data_list$K
#'
#'   # Use default specification for alpha
#'   # small code duplcation in check - how to abstract
#'   if (is.null(out$alpha)) {
#'     out$alpha <- default_alpha(K)
#'   }
#'
#'   # Use default specification for beta
#'   if (is.null(out$beta)) {
#'     beta_default <- matrix(1, nrow = K, ncol = K)
#'     diag(beta_default) <- 2.5 * K
#'
#'     out$beta <-  beta_default
#'   }
#'
#'   validate_alpha(out$alpha, K)
#'
#'   # valdiate beta parameter
#'   if (!all(dim(out$beta) == rep(K, 2))) {
#'     stop("Beta must be of dimension ", K, " x ", K, call. = FALSE)
#'   }
#'
#'   out
#'
#' }
#'
#'
#' #' Converts the passed priors for the Hierachical Dawid and Skene model
#' #'
#' #' See parse_priors for details
#' #'
#' #' @param model rater model
#' #' @param data_list data converted to Stan list format see \code{parse_data}
#' parse_priors_hierds <- function(model, data_list) {
#'   out <- model$parameters
#'   K <- data_list$K
#'
#'   # Use default specification for alpha
#'   if (is.null(out$alpha)) {
#'     out$alpha <- default_alpha(K)
#'   }
#'
#'   validate_alpha(out$alpha, K)
#'
#'   out
#'
#' }

#' #' Checks that a possible prior on alpha is valid
#' #'
#' #' Checks that the length of the prior on alpha == K and errors if not
#' #'
#' #' @param alpha prior parameter for pi
#' #' @param K number of categories in the data
#' validate_alpha <- function(alpha, K) {
#'   if (length(alpha) != K) {
#'     stop("Alpha must of length ", K, ", the number of categories in the data",
#'          call. = FALSE)
#'   }
#'
#' }

#' #' Creates an alpha according to the default prior specification
#' #'
#' #' @param K number of categories in the data
#' #'
#' default_alpha <- function(K) {
#'   rep(3, K)
#' }

# TODO fix typo

#' Creates inits for the stan MCMC chains
#'
#' @param model rater model
#' @param data_list data in list form
#'
#' @details If the model is of type Dawid and Skene creates inits favouring
#' good raters. This is to prevent label switching see: TODO. If the model is
#' not Dawid and Skene then the inits are set randomly
creat_inits <- function(model, stan_data) {
  # better to have another short unique id...
  switch(get_file(model),
    "multinomial" = multinomial_inits(stan_data$K, stan_data$J),
    "dawid_skene" = dawid_skene_inits(stan_data$K, stan_data$J),
    "hierarchical_dawid_skene" = "random",
    stop("Unsupported model type", call. = FALSE))
}

multinomial_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- array(0.2 / (K - 1), c(K, K))
  diag(theta_init) <- 0.8
  function(n) list(theta = theta_init, pi = pi_init)
}

dawid_skene_inits <- function(K, J) {
  pi_init <- rep(1/K, K)
  theta_init <- array(0.2 / (K - 1), c(J, K, K))
  for (j in 1:J) {
      diag(theta_init[j, ,]) <- 0.8
  }
  function(n) list(theta = theta_init, pi = pi_init)
}


#' #' Check that the passed data is in the appropriate format
#' #'
#' #' Checks whether the passed data is in the long format. That is: three columns
#' #' item index, annotator index, annotation. This function checks that the data
#' #' has three columns and that they are all numeric.
#' #'
#' #' @param model model object
#' #' @param data Data passed to \code{mcmc} hopefully in 'long' format
#' #'
#' validate_data <- function(model, data) {
#'
#'   defualt_msg <- "Data must be in long format!"
#'
#'   if (!(ncol(data)  %in% c(2, 3))) {
#'     stop("Data must be in 'long' format", call. = FALSE)
#'   }
#'
#'   if (is.multinomial(model)) {
#'
#'       if(!(ncol(data) == 2)) {
#'
#'         multinom_msgs <- c("For the multinomial model data must be in the following format:\n",
#'                            "Column 1: Item index\n",
#'                            "Column 2: Annotation - rating given\n")
#'
#'         stop(paste(multinom_msgs, sep = ""), call. = FALSE)
#'       }
#'
#'   } else {
#'
#'     if(!(ncol(data) == 3)) {
#'
#'         other_msgs <- c("For the partially pooled or unpooled models data ",
#'                         "must be in the following format:\n",
#'                         "Column 1: Item index\n",
#'                         "Column 2: Annotator - rater\n",
#'                         "Column 2: Annotation - rating given\n")
#'
#'         stop(other_msgs, call. = FALSE)
#'       }
#'
#'   }
#'
#'   inds <- numeric(0)
#'   for (i in 1:ncol(data)){
#'     if (!is.numeric(data[, i])) {
#'       inds <- c(inds, i)
#'     }
#'   }
#'
#'   if (length(inds) > 0) {
#'     stop("Columns ", paste(inds, collapse = ", "), " are not numeric",
#'     call. = FALSE)
#'   }
#'
#' }
