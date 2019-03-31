#' print a fit model object
#'
#' @param x fit object to be printed
#' @param ... other args passed to the function
#'
#' @export
print.rater_model <- function(x, ...) {

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
#'
#' @param object model object
#' @param ... other args passed to the function
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

is.multinomial <- function(m) {
  inherits(m, "multinomial")
}

is.model <- function(m) {
  inherits(m, "rater_model")
}
