#' print a rater_model object
#'
#' @param x a rater_model object
#' @param ... other args
#'
#' @export
print.rater_model <- function(x, ...) {
  cat(get_name(x), "\n\n")
  params <- get_parameters(x)
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

is.multinomial <- function(m) {
  inherits(m, "multinomial")
}

is.rater_model <- function(m) {
  inherits(m, "rater_model")
}
