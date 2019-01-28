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
dawid_skene <- function(alpha = NULL,
                        beta  = NULL) {

  obj <- list(parameters = list(alpha = alpha,
                                beta  = beta))

  class(obj) <- c("dawid_skene", "model")

  obj
}

#' @rdname models
#' @export
#'
hier_dawid_skene <- function(alpha = NULL) {

  # Note: this does not allow the user to change the N(0, 1) hyperpriors

  obj <- list(parameters = list(alpha = alpha))

  class(obj) <- c("hier_dawid_skene", "model")

  obj
}

#' @rdname models
#' @export
#'
#' @details theta is of different dimension to dawid and skene model
#'
multinomial <- function(alpha = NULL,
                        beta  = NULL) {

  obj <- list(parameters = list(alpha = alpha,
                                beta  = beta ))

  class(obj) <- c("multinomial", "model")

  obj
}


