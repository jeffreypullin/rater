#' print a fit object
#'
#' @param x fit object to be printed
#' @param ... other args passed to the function
#'
#' @export
print.fit <- function(x, ...) {
  fit <- x

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
#' @param x fit object
#' @param ... other arguments to be passed
#' theta or raters)
#'
#' @details ... must contain the param argument which tell the function
#' what to extract from the fit object. It may also contain the which argument
#' which controls which raters confusion matrices will be plotted.
#'
#' @export
plot.fit <- function(x, ...) {

  fit <- x
  dots <- list(...)

  if (length(dots) == 0) {
    stop("The type of plot must be specified", call. = FALSE)
  }

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
#'
#' @param object object of type rater fit
#' @param ... other args passed to function
#'
#' @export
summary.fit <- function(object, ...) {
  fit <- object
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
#' @param x fit object
#' @param ... other arguments to be passed
#' theta or raters)
#'
#' @details ... must contain the param argument which tell the function
#' what to extract from the fit object. It may also contain the which argument
#' which controls which raters confusion matrices will be plotted.
#'
#' @export
extract.fit <- function(x, ...) {

  fit <- x
  dots <- list(...)

  if (length(dots) == 0) {
    stop("The param to extract must be specified", call. = FALSE)
  }

  which <- dots$which
  param <- match.arg(dots$param, c("theta", "raters",
                                   "pi", "prevalance",
                                   "z", "latent_class"))

  if (param %in% c("theta", "raters")) {

    out <- extract_raters(fit, which = which)

  } else if (param %in% c("pi", "prevalance")) {

    out <- extract_prevalance(fit)

  } else if (param %in% c("z", "latent_class")) {

    out <- extract_latent_class(fit)

  }

  out
}

