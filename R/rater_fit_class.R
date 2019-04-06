#' Make a mcmc_rater fit object
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

#' Print a mcmc_fit object
#'
#' @param x fit object to be printed
#' @param ... other args passed to the function
#'
#' @export
#'
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

#' Plot a rater_fit object
#'
#' @param x fit object
#' @param type the type of plot
#' @param ... other args
#'
#' @details ... must contain the param argument which tell the function
#' what to extract from the fit object. It may also contain the which argument
#' which controls which raters confusion matrices will be plotted.
#'
#' @export
#'
plot.rater_fit <- function(x, type = "theta", ...) {
  dots <- list(...)
  which <- dots$which

  # see utils.R for names
  type <- match.arg(type, plot_names)

  switch(type,
    "theta" = plot_theta(x, which = which),
    "rater" = plot_theta(x, which = which),
    "z" = plot_z(x),
    "latent_class" = plot_z(x),
    # luckily p will fall through correctly
    "pi" = plot_pi(x),
    "prevelance" = plot_pi(x),
    stop("Invalid type argument", call. = FALSE)
  )
}

#' Summary of fit
#'
#' @param object object of type rater fit
#' @param ... other args passed to function
#'
#' @export
#'
summary.mcmc_fit <- function(object, ...) {
  cat(get_name(get_model(object)), "with MCMC draws")
}

#' Check if object is of type fit
#' @param x object
#'
is.mcmc_fit <- function(x) {
  inherits(x, "mcmc_fit")
}

is.rater_fit <- function(x) {
  inherits(x, "rater_fit")
}

#' Generic to extract theta from a fit
#'
#' @param fit an object
#' @param which which rater to select
#' @param ... extra args
#'
#' @export
#'
extract_theta <- function (fit, which = NULL, ...) {
   UseMethod("extract_theta", fit)
}

#' Generic to extract pi from a fit
#'
#' @param fit an object
#' @param ... extra stuff
#'
#' @export
#'
extract_pi <- function(fit, ...) {
  UseMethod("extract_pi", fit)
}

#' Generic to extract z (latent class) from a fit
#'
#' @param fit an object
#' @param ... extra stuff
#'
#' @export
#'
extract_z <- function(fit, ...) {
  UseMethod("extract_z", fit)
}

#' Function to extract prevalence from a fit
#'
#' @param fit an object
#' @param ... extra stuff
#'
#' @export
#'
extract_prevalence <- extract_pi

#' Generic to extract (latent class) from a fit
#'
#' @param fit an object
#' @param ... extra stuff
#'
#' @export
#'
extract_latent_class <- extract_z

#' Function to extract raters from a fit
#'
#' @param fit an object
#' @param which which rater to select
#' @param ... extra args
#'
#' @export
#'
extract_raters <- extract_theta

get_model <- function(f) {
  f$model
}

get_draws <- function(f) {
  f$draws
}

# bit of a hack reusing get_data - should it be generic?
