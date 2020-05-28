#' Make a mcmc_rater fit object
#'
#' @param model a rater model
#' @param samples a stanfit object
#' @param data the data used to fit the model
#' @param data_format The format of the data used to fit the model
#'
#' @return a rater fit object
#'
#' @noRd
new_mcmc_fit <- function(model, samples, data, data_format) {
  new <- list(model = model,
              samples = samples,
              data = data,
              data_format = data_format)
  class(new) <- c("mcmc_fit", "rater_fit")
  new
}

#' Make a optim_rater fit object
#'
#' @param model a rater model
#' @param estimates a stan optimisation object
#' @param data the data used to fit the model
#' @param data_format The format of the data used to fit the model
#'
#' @return a rater fit object
#'
#' @noRd
new_optim_fit <- function(model, estimates, data, data_format) {
  new <- list(model = model,
              estimates = estimates,
              data = data,
              data_format = data_format)
  class(new) <- c("optim_fit", "rater_fit")
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
  print(get_samples(x))
  cat("\n")
  options(max.print = max.print_default)
}
# nocov end

#' Print a optim_fit object
#'
#' @param x fit object to be printed
#' @param ... other args passed to the function
#'
#' @export
#'
# nocov start
print.optim_fit <- function(x, ...) {
  cat("Fit method: Optimisation\n\n")

  cat("Model:\n\n")
  print(get_model(x))
  cat("\n")

  cat("Estimates:\n")

  max.print_default <- options("max.print")[[1]]
  options(max.print = 10)
  est_data <- data.frame(get_estimates(x)$par)
  colnames(est_data) <- NULL
  print(est_data)
  cat("\n")
  options(max.print = max.print_default)

  cat(paste0("Log probability: ", round(x$estimates$value, 4), "\n"))
  cat(paste0("Fit converged: ", as.logical(x$estimates$return_code - 1), "\n"))
}
# nocov end

#' Plot a rater_fit object
#'
#' @param x A rater_fit object
#' @param pars Which parameters to plot. Can use both mathematical or
#'   natural names.
#' @param ... Other arguments. This should contain the which argument for
#'   theta plots.
#'
#' @return If one parameter is requested a ggplot2 plot. If multiple parameters
#'   are requested a list of ggplot2 plots.
#'
#' @export
#'
plot.rater_fit <- function(x, pars = c("pi", "theta", "z"), ...) {
  dots <- list(...)
  which <- dots$which

  plot_names <- c("theta", "raters", "pi",
                  "prevalence", "z", "latent_class")

  plots <- list()
  for (i in seq_along(pars)) {
    par <- match.arg(pars[[i]], plot_names)
    plots[[i]] <- switch(par,
      "theta" = plot_theta(x, which = which),
      "raters" = plot_theta(x, which = which),
      "z" = plot_z(x),
      "latent_class" = plot_z(x),
      # Luckily "p" will fall through correctly.
      "pi" = plot_pi(x),
      "prevelance" = plot_pi(x),
      stop("Invalid pars argument", call. = FALSE)
    )
  }

  # Ensure that we return a bare plot if we are only returning one plot.
  if (length(plots) == 1L) {
    out <- plots[[1]]
  } else {
    out <- plots
  }
  out
}

#' Summary of mcmc fit
#'
#' @param object object of type rater fit
#' @param ... other args passed to function
#'
#' @method summary mcmc_fit
#' @export
summary.mcmc_fit <- function(object, ...) {
  cat(get_name(get_model(object)), "with MCMC draws")
}

#' Summary of optim fit
#'
#' @param object object of type rater fit
#' @param ... other args passed to function
#'
#' @method summary optim_fit
#' @export
summary.optim_fit <- function(object, ...) {
  cat(get_name(get_model(object)), "with MAP estimates")
}

#' Check if object is of type fit
#' @param x object
#'
is.mcmc_fit <- function(x) {
  inherits(x, "mcmc_fit")
}

is.optim_fit <- function(x) {
  inherits(x, "optim_fit")
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

#' Get the posterior samples from a rater mcmc fit object
#'
#' @fit A rater mcmc fit object
#'
#' @noRd
get_samples <- function(fit) {
  fit$samples
}

get_estimates <- function(f) {
  f$estimates
}


# bit of a hack reusing get_data - should it be generic?
