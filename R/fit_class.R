#' print a fit object
#' @export
print.fit <- function(fit) {

  cat("Model:\n\n")
  print(fit$model)
  cat("\n")

  max.print_default <- options("max.print")[[1]]
  options(max.print = 80)

  cat("Samples:\n\n")
  print(fit$draws)
  cat("\n")

  options(max.print = max.print_default)

}
