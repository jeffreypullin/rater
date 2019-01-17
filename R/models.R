#' @name models
#'
#' @title Probablistic models of categorical data annotation
#' @description Functions to set up models and change their prior
#'   parameters for use in \code{\link{mcmc}()}.
#'
#' @return a \code{model} object that can be passed to \code{\link{mcmc}}.

NULL

#' @rdname samplers

dawid_skene <- function(alpha = NULL,
                        beta = NULL) {

  obj <- list(parameters = list(alpha = alpha,
                                beta  = beta))

  class(obj) <- c("dawid_skene", "model")

  obj
}


print.model <- function(x, print_code = FALSE) {

  cat(get_name(x))
  #cat("the model")

  #cat(, "\n\n", sep = "")

  #params <- x$parameters
  #cat("Prior parameters:\n\n")
  #for (i in 1:length(params)) {
  #  if (!is.null(params[[i]])) {
  #    cat(paste0(names(params)[[i]], ":"), "\n\n")
  #    print(params[[i]])
  #    cat("\n")
  #  }
  #}

  #if (print_code) {
  #  cat("Stan code:\n")
  #  cat("\n")
  #  # TODO dispatch here
  #  path <- "./src/stan_files/dawid_skene.stan"
  #  writeLines(readLines(path))
  #}

}
