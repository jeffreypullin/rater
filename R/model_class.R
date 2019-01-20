#' print a fit model object
#' @export
print.model <- function(x, print_code = FALSE) {
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
summary.model <- function(model) {
  cat(get_name(model))
}
