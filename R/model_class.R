#' print a fit model object
#' @export
print.model <- function(x, ...) {

  cat(get_name(x), "\n\n")
  params <- x$parameters

  cat("Prior parameters:\n\n")

  for (i in 1:length(params)) {
    cat(paste0(names(params)[[i]], ":"))

    if (!is.null(params[[i]])) {

      cat("\n \n")
      print(params[[i]])
      cat("\n")

    } else {
      cat(" default\n")
    }
  }
}

#' Summary of model
#' @param model
#'
#' @export
summary.model <- function(x, ...) {
  model <- x
  cat(get_name(model))
}

#' Check if model is of type Dawid and skene
#' @param model object of type model
#' @export
is.dawid_skene <- function(model) {
  inherits(model, "dawid_skene")
}

#' Check if model is of type Hierarchical Dawid and skene
#' @param model object of type model
#' @export
is.hier_dawid_skene <- function(model) {
  inherits(model, "hier_dawid_skene")
}

is.multinomial <- function(model) {
  inherits(model, "multinomial")
}

#' Check if object is of type model
#' @param x object
#' @export
is.model <- function(x) {
  inherits(x, "model")
}
