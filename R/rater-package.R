#' The 'rater' package.
#'
#' @description  Fit statistical models based on the Dawid-Skene model to repeated
#'  categorical rating data. Full Bayesian inference for these models is
#'  supported through the Stan modelling language. rater also allows the user to
#'  extract and plot key parameters of these models.
#'
#' @docType package
#' @name rater-package
#' @useDynLib rater, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom RcppParallel CxxFlags RcppParallelLibs
#' @importFrom rstan sampling
#' @aliases rater-package
#' @references
#' Stan Development Team (2018). RStan: the R interface to Stan. R package version 2.18.2. http://mc-stan.org
#'
NULL
