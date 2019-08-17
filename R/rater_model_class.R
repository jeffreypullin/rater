#' print a rater_model object
#'
#' @param x a rater_model object
#' @param ... other args
#'
#' @export
print.rater_model <- function(x, ...) {
  cat(get_name(x), "\n\n")
  pars <- get_parameters(x)
  cat("Prior parameters:\n\n")
  for (i in 1:length(pars)) {
    cat(paste0(names(pars)[[i]], ":"))
    if (!is.null(pars[[i]])) {
      cat("\n \n")
      print(pars[[i]])
      cat("\n")
    } else {
      cat(" default\n")
    }
  }
}

#' summarise a rater_model
#'
#' @param object a rater_model object
#' @param ... other args
#'
#' @export
summary.rater_model <- function(object, ...) {
  cat(get_name(object))
}

is.dawid_skene <- function(model) {
  inherits(model, "dawid_skene")
}

is.hier_dawid_skene <- function(m) {
  inherits(m, "hier_dawid_skene")
}

is.rater_model <- function(m) {
  inherits(m, "rater_model")
}

#' Gets the long name of a model
#'
#' @param m object of type rater_model
#'
get_name <- function(m) {
  m$name
}

#' Gets stan file name of a model
#'
#' @param m object of type rater_model
#'
get_file <- function(m) {
  m$file
}

get_K <- function(m) {
  m$K
}

get_parameters <- function(m) {
  m$parameters
}
