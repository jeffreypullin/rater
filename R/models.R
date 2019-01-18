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
#' @param alpha
#' @param beta
#'
dawid_skene <- function(alpha = NULL,
                        beta = NULL) {

  obj <- list(parameters = list(alpha = alpha,
                                beta  = beta))

  class(obj) <- c("dawid_skene", "model")

  obj
}

#' print a fit model object
#' @export
print.model <- function(x, print_code = FALSE) {
  cat("\n\n")
  cat(get_name(x), "\n\n")
  params <- x$parameters

  cat("Prior parameters:\n\n")

  for (i in 1:length(params)) {
    cat(names(params))

    if (!is.null(params[[i]])) {

      cat("\n\n")
      print(params[[i]])
      cat("\n")

    } else {
      cat(" default\n\nn")
    }
  }
}
