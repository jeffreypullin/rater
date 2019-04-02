#' make a mcmc rater fit object
#'
#' @param model a rater model
#' @param draws a stanfit object
#' @param data the data used to fit the model
#'
#' @return a rater fit object
#'
new_mcmc_fit <- function(model, draws, data) {
  new <- list(model = model, draws = draws, data = data)
  class(new) <- c("mcmc_fit", "rater_fit")
  new
}

#' print a fit object
#'
#' @param x fit object to be printed
#' @param ... other args passed to the function
#'
#' @export
# nocov start
print.mcmc_fit <- function(x, ...) {
  cat("Model:\n\n")
  print(get_model(x))
  cat("\n")

  # stop print/show.stanfit from going crazy...
  max.print_default <- options("max.print")[[1]]
  options(max.print = 80)
  cat("Samples:\n\n")
  print(get_draws(x))
  cat("\n")
  options(max.print = max.print_default)
}
# nocov end

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
plot.rater_fit <- function(x, ...) {

  fit <- x
  dots <- list(...)

  if (length(dots) == 0) {
    stop("The type of plot must be specified", call. = FALSE)
  }

  if (is.null(names(dots))) {
    stop("Please specify type of plot with type = ", call. = FALSE)
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
summary.mcmc_fit <- function(object, ...) {
  cat(get_name(get_model(object)), "with MCMC draws")
}

#' Check if object is of type fit
#' @param x object
#' @export
is.mcmc_fit <- function(x) {
  inherits(x, "mcmc_fit")
}

is.rater_fit <- function(x) {
  inherits(x, "rater_fit")
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

# hey - we can set our own signature!
extract_theta <- function (x, which = NULL) {
   UseMethod("extract_theta", x)
}

# use some judicoious ifs and helpers ...
# need to dispatch this on the type of the model - somehow...
# something like - class == c("dawid_skene", "mcmc_fit" , "rater_fit"

extract_pi <- function(fit, ...) {
  UseMethod("extract_pi", fit)
}

extract_z <- function(fit, ...) {
  UseMethod("extract_z", fit)
}

# need to make judicoius use of next method


get_model <- function(f) {
  f$model
}

get_draws <- function(f) {
  f$draws
}

# bit of a hack reusing get_data - should it be generic?
