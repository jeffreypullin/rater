#' Print a `rater_model` object.
#'
#' @param x A `rater_model` object.
#' @param ... Other arguments
#'
#' @examples
#' mod <- dawid_skene()
#' print(mod)
#'
#' @export
#'
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

#' Summarise a `rater_model`.
#'
#' @param object A `rater_model` object.
#' @param ... Other arguments.
#'
#' @examples
#' mod <- dawid_skene()
#' summary(mod)
#'
#' @method summary rater_model
#'
#' @export
#'
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

is.class_conditional_dawid_skene <- function(m) {
  inherits(m, "class_conditional_dawid_skene")
}

#' Gets the long name of a model
#'
#' @param m Object of type `rater_model`.
#'
#' @noRd
#'
get_name <- function(m) {
  m$name
}

#' Gets the stan file name of a model
#'
#' @param m object of type `rater_model`.
#'
#' @noRd
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
