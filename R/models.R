#' @name models
#'
#' @title Probabilistic models of repeated categorical rating
#' @description Functions to set up models and change their prior
#'   parameters for use in [rater()].
#'
#' @return a `model` object that can be passed to [rater()].
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
#' @param beta_1 First on diagonal prior probability parameter
#' @param beta_2 Second on diagonal prior probability parameter for theta
#'
class_conditional_dawid_skene <- function(alpha = NULL,
                                          beta_1 = NULL,
                                          beta_2 = NULL) {
  parameters <- list(alpha = alpha, beta_1 = beta_1, beta_2 = beta_2)
  validate_parameters(parameters)
  m <- list(parameters = parameters,
            name = "Bayesian Class conditional Dawid and Skene Model",
            file = "class_conditional_dawid_skene",
            K = compute_K(parameters))
  class(m) <- c("class_conditional_dawid_skene", "rater_model")
  m
}

#' Validate passed parameters
#'
#' Checks the parameter are of the appropriate type/form and that they are
#' self consistent
#'
#' @param pars a (named) list of parameters
#'
validate_parameters <- function(pars) {
  beta <- pars$beta
  alpha <- pars$alpha
  # noone should be using 1:n syntax for prior for al
  if (!is.null(alpha) && !is.numeric(alpha)) {
      stop("alpha must be a numeric vector", call. = FALSE)
  }
  if (!is.null(beta) && !is.matrix(beta)) {
      stop("beta must be a square numeric matrix", call. = FALSE)
  }
  # need to test both are not NULL!!!
  if (!is.null(alpha) && !is.null(beta) && (length(alpha) != unique(dim(beta)))) {
      stop("alpha and beta must have the same dimensions", call. = FALSE)
  }
}

# takes the list of parameters - alpha/beta
compute_K <- function(pars) {
   # no K values specified
  if (is.null(unlist(pars))) {
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
