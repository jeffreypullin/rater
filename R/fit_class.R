#' print a fit object
#' @export
print.fit <- function(fit, ...) {

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
#'
#' @param fit a fit object
#' @param type what sort of plot should be plotted
#' @param which Which of the raters' matrices to extract (only used for type =
#' theta or raters)
#'
#' @export
plot.fit <- function(fit, ...) {
  dots <- list(...)

  which <- dots$which
  type <- match.arg(dots$type, c("theta", "raters",
                                 "pi", "prevalance",
                                 "z", "latent_class"))

  if (type %in% c("theta", "raters")) {

    plot <- plot_raters(fit, which = which)

  } else if (type %in% c("pi", "prevalance")) {

    plot <- plot_prevalance(fit)

  } else if (type %in% c("z", "latent_class")) {

    plot <- plot_latent_class(fit)

  }

  plot
}

#' Summary of fit
#' @param fit object of type rater fit
#' @export
summary.fit <- function(fit, ...) {
  cat(get_name(fit$model), "with MCMC draws")
}

#' Check if object is of type fit
#' @param x object
#' @export
is.fit <- function(x) {
  inherits(x, "fit")
}


#' Add an extract method
#' @param x an object
#' @param ... other stuff passed to extract
#' @export
extract <- function (x, ...) {
   UseMethod("extract", x)
}

#' Extract method for a rater fit object
#'
#' Extract different paramter estimates from a fit object
#'
#' @param fit a fit object
#' @param type what sort of plot should be plotted
#' @param which Which of the raters' matrices to extract (only used for type =
#' theta or raters)
#'
#' @export
extract.fit <- function(fit, ...) {
  dots <- list(...)

  which <- dots$which
  param <- match.arg(dots$param, c("theta", "raters",
                                   "pi", "prevalance",
                                   "z", "latent_class"))

  #if (missing(param)) {
  #  stop("The parameter to be extracted must be specified (through param = )",
  #       call. = FALSE)
  #}

  if (param %in% c("theta", "raters")) {

    out <- extract_raters(fit, which = which)

  } else if (param %in% c("pi", "prevalance")) {

    out <- extract_prevalance(fit)

  } else if (param %in% c("z", "latent_class")) {

    out <- extract_latent_class(fit)

  }

  out
}

