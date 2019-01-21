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


#' Plot a fit object
#' @param fit a fit object
#' @param type what sort of plot should be plotted
#'
#' @export
plot.fit <- function(fit, type = c("theta", "raters",
                                   "pi", "prevalance",
                                   "z", "latent_class")) {
  type <- match.arg(type)

  if (type %in% c("theta", "raters")) {

    plot <- plot_raters(fit)

  } else if (type %in% c("pi", "prevalance")) {

    plot <- plot_prevalance(fit)

  } else if (type %in% c("z", "latent_class")) {

    plot <- plot_latent_class(fit)

  }

  plot
}

#' Summary of fit
#' @param fit
#'
#' @export
summary.fit <- function(fit) {
  cat(get_name(fit$model), "with MCMC draws")
}
