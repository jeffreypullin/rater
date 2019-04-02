#' @name models
#'
#' @title Probablistic models of categorical data annotation
#' @description Functions to set up models and change their prior
#'   parameters for use in \code{\link{mcmc}()}.
#'
#' @return a \code{model} object that can be passed to \code{\link{mcmc}}.
#'
NULL

#' @rdname models
#' @export
#'
#' @param alpha prior parameter for pi
#' @param beta prior parameter for theta
#'
dawid_skene <- function(alpha = NULL, beta = NULL) {
  parameters <- list(alpha = alpha, beta = beta)
  validate_parameters(parameters)
  m <- list(parameters = parameters,
            name = "Bayesian Dawid and Skene Model",
            file = "dawid_skene",
            K = compute_K(parameters))
  class(m) <- c("dawid_skene", "rater_model")
  m
}

#' @rdname models
#' @export
#'
hier_dawid_skene <- function(alpha = NULL) {
  # Note: this does not allow the user to change the N(0, 1) hyperpriors
  parameters <- list(alpha = alpha)
  validate_parameters(parameters)
  m <- list(parameters = parameters,
            name = "Bayesian Hierarchical Dawid and Skene Model",
            file = "hierarchical_dawid_skene",
            K = compute_K(parameters))
  class(m) <- c("hier_dawid_skene", "rater_model")
  m
}

#' @rdname models
#' @export
#'
multinomial <- function(alpha = NULL, beta = NULL) {
  parameters <- list(alpha = alpha, beta = beta)
  validate_parameters(parameters)
  m <- list(parameters = parameters,
            name = "Bayesian Multinomial (Annotator pooled) Model",
            file = "multinomial",
            K = compute_K(parameters))
  class(m) <- c("multinomial", "rater_model")
  m
}

#' Validate passed parameters
#'
#' Checks the parameter are of the appropritate type/form and that they are
#' self consistent
#'
#' @param parameters a (named) list of parmeters
#'
validate_parameters <- function(pars) {
  beta <- pars$beta
  alpha <- pars$alpha
  if (!is.null(alpha) & class(alpha) != "numeric") {
      stop("alpha must be a numeric vector", call. = FALSE)
  }
  if (!is.null(beta) & (class(beta) != "matrix" || length(unique(beta)) != 1)) {
      stop("Beta must be a square numeric matrix", call. = FALSE)
  }
  if (!is.null(unlist(pars)) && (length(alpha) != unique(dim(beta)))) {
      stop("Alpha and beta must have the same dimensions", call. = FALSE)
  }
}

# takes the list of parameters - alpha/beta
compute_K <- function(pars) {
  if (is.null(unlist(pars))) {
    # no K values specified
    # not null to allow comaprsion to numerics in mcmc
    NULL
  } else {
    ks <- c(length(pars$alpha), unique(dim(pars$beta)))
    # Note:
    # we assume here that the consistency of parameter
    # dimensions has already been checked
    # stopifnot(length(unique(ks)) != 1)
    ks[ks > 0]
  }
}
