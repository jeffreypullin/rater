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
                        beta = NULL) {

  obj <- list(parameters = list(alpha = alpha,
                                beta  = beta))

  class(obj) <- c("dawid_skene", "model")

  obj
}
