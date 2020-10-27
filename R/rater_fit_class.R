#' Make an MCMC rater fit object
#'
#' @param model A rater model; an object of class `rater_model`.
#' @param samples A stanfit object containing posterior samples.
#' @param stan_data The data used to fit the model in the form passed to Stan.
#' @param data_format The format of the data used to fit the model.
#'
#' @return An object of class `c("mcmc_fit", "rater_fit")`
#'
#' @noRd
#'
new_mcmc_fit <- function(model, samples, stan_data, data_format) {
  new <- list(model = model,
              samples = samples,
              stan_data = stan_data,
              data_format = data_format)
  class(new) <- c("mcmc_fit", "rater_fit")
  new
}

#' Make an optimisation rater fit object
#'
#' @param model A rater model; an object of class `rater_model`.
#' @param estimates A stanfit object containing parameter estimates.
#' @param stan_data The data used to fit the model in the form passed to Stan.
#' @param data_format The format of the data used to fit the model.
#'
#' @return An object of class `c("optim_fit", "rater_fit")`
#'
#' @noRd
#'
new_optim_fit <- function(model, estimates, stan_data, data_format) {
  new <- list(model = model,
              estimates = estimates,
              stan_data = stan_data,
              data_format = data_format)
  class(new) <- c("optim_fit", "rater_fit")
  new
}

#' Print a `mcmc_fit` object
#'
#' @param x An object of class `mcmc_fit`.
#' @param ... Other arguments.
#'
#' @export
#'
# nocov start
print.mcmc_fit <- function(x, ...) {
  cat(get_name(get_model(x)), "with MCMC draws.\n")
}
# nocov end

#' Print a `optim_fit` object
#'
#' @param x An object of class `optim_fit`.
#' @param ... Other arguments.
#'
#' @export
#'
# nocov start
print.optim_fit <- function(x, ...) {
  cat(get_name(get_model(x)), "with MAP estimates.\n")
}
# nocov end

#' Plot a `rater_fit` object
#'
#' @param x An object of class `rater_fit`.
#' @param pars A character vector of the names of the parameters to plot. By
#'   default: `c("pi", "theta", "class_probabilities")`.
#' @param ... Other arguments. This should contain the which argument for
#'   theta plots.
#'
#' @return If one parameter is requested a ggplot2 plot. If multiple parameters
#'   are requested a list of ggplot2 plots.
#'
#' @examples
#'
#' \donttest{
#' fit <- rater(anesthesia, "dawid_skene")
#'
#' # Plot all the parameters.
#' plot(fit)
#'
#' # Select which parameters to plot.
#' plot(fit, pars = "pi")
#'
#' }
#'
#' @export
#'
plot.rater_fit <- function(x,
                           pars = c("pi", "theta", "class_probabilities"),
                           ...) {
  dots <- list(...)
  which <- dots$which

  plot_names <- c("theta", "raters", "pi",
                  "prevalence", "class_probabilities", "latent_class")

  plots <- list()
  for (i in seq_along(pars)) {
    par <- match.arg(pars[[i]], plot_names)
    plots[[i]] <- switch(par,
      "theta" = plot_theta(x, which = which),
      "raters" = plot_theta(x, which = which),
      "class_probabilities" = plot_class_probabilities(x),
      "latent_class" = plot_class_probabilities(x),
      # Luckily "p" will fall through correctly.
      "pi" = plot_pi(x, 0.9),
      "prevalence" = plot_pi(x, 0.9),
      "z" = stop("Cannot plot z directly.", call. = FALSE),
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

#' Summarise a `mcmc_fit` object
#'
#' @param object An object of class `mcmc_fit`.
#' @param n_pars The number of pi/theta parameters and z 'items' to display.
#' @param ... Other arguments passed to function.
#'
#' @method summary mcmc_fit
#'
#' @importFrom utils head
#'
#' @export
#'
summary.mcmc_fit <- function(object, n_pars = 8, ...) {
  fit <- object

  # Prepare pi.
  pi_est <- pi_to_long_format(pi_point_estimate(fit))
  colnames(pi_est) <- "mean"
  pi_interval <- posterior_interval(fit, pars = "pi")
  pi_mcmc_diagnostics <- mcmc_diagnostics(fit, pars = "pi")
  pi <- cbind(pi_est, pi_interval, pi_mcmc_diagnostics)

  # Prepare theta.
  theta_est <- theta_to_long_format(theta_point_estimate(fit))
  colnames(theta_est) <- "mean"
  theta_interval <- posterior_interval(fit, pars = "theta")
  theta_mcmc_diagnostics <- mcmc_diagnostics(fit, pars = "theta")
  theta <- cbind(theta_est, theta_interval, theta_mcmc_diagnostics)

  pars <- rbind(pi, theta)

  # Prepare z.
  class_probs <- class_probabilities(fit)
  colnames(class_probs) <- sprintf("Pr(z = %s)", 1:ncol(class_probs))
  z <- z_to_long_format(apply(class_probs, 1, which.max))
  colnames(z) <- "MAP"
  z_out <- cbind(z, class_probs)

  # Do the actual printing:

  cat("Model:\n")
  print(get_model(fit))

  cat("\nFitting method: MCMC\n")

  cat("\npi/theta samples:\n")
  print(round(utils::head(pars, n_pars), 2))
  # pars is a matrix where each row is a parameter.
  n_remaining <- nrow(pars) - n_pars
  cat("# ... with", n_remaining, "more parameters\n")

  cat("\nz:\n")
  print(round(head(z_out, n_pars), 2))
  n_remaining_z <- nrow(z) - n_pars
  cat("# ... with", n_remaining_z, "more items\n")

}

#' Summarise an `optim_fit` object
#'
#' @param object An object of class `optim_fit`.
#' @param n_pars The number of pi/theta parameters and z 'items' to display.
#' @param ... Other arguments passed to function.
#'
#' @method summary optim_fit
#'
#' @importFrom utils head
#'
#' @export
#'
summary.optim_fit <- function(object, n_pars = 8, ...) {
  x <- object
  fit <- object

  # Prepare pi.
  pi <- pi_to_long_format(pi_point_estimate(fit))
  colnames(pi) <- "mean"

  # Prepare theta.
  theta <- theta_to_long_format(theta_point_estimate(fit))
  colnames(theta) <- "mean"

  pars <- rbind(pi, theta)

  # Prepare z.
  class_probs <- class_probabilities(fit)
  colnames(class_probs) <- sprintf("Pr(z = %s)", 1:ncol(class_probs))
  z <- z_to_long_format(apply(class_probs, 1, which.max))
  colnames(z) <- "MAP"
  z_out <- cbind(z, class_probs)

  # Do the actual printing:

  cat("Model:\n")
  print(get_model(fit))

  cat("\nFitting method: Optimisation\n")

  cat("\npi/theta estimates:\n")
  print(round(head(pars, n_pars), 2))
  # pars is a *list*
  n_remaining <- length(pars) - n_pars
  cat("# ... with", n_remaining, "more parameters\n")

  cat("\nz:\n")
  print(round(utils::head(z_out, n_pars), 2))
  n_remaining_z <- nrow(z) - n_pars
  cat("# ... with", n_remaining_z, "more items\n")

  cat("\n")
  cat(paste0("Log probability: ", round(x$estimates$value, 4), "\n"))
  cat(paste0("Fit converged: ", as.logical(x$estimates$return_code - 1), "\n"))
}

#' Convert a rater_fit object to a {coda} `mcmc.list` object.
#'
#' @param fit A rater_fit object.
#'
#' @return A {coda} mcmc.list object.
#'
#' @importFrom rstan As.mcmc.list
#'
#' @examples
#'
#' \donttest{
#'
#' # Fit a model using MCMC (the default).
#' mcmc_fit <- rater(anesthesia, "dawid_skene")
#'
#' # Convert it to an mcmc.list
#' rater_mcmc_list <- as_mcmc.list(mcmc_fit)
#'
#' }
#'
#' @export
#'
as_mcmc.list <- function(fit) {

  if (!inherits(fit, "rater_fit")) {
    stop("`as_mcmc.list` must be passed a rater fit object.", call. = FALSE)
  }

  if (inherits(fit, "optim_fit")) {
    stop("Cannot convert a optimisation fit to a mcmc.list object",
         call. = FALSE)
  }

  # We must have a mcmc_fit, rater_fit!
  rstan::As.mcmc.list(fit$samples)
}

is.mcmc_fit <- function(x) {
  inherits(x, "mcmc_fit")
}

is.optim_fit <- function(x) {
  inherits(x, "optim_fit")
}

is.rater_fit <- function(x) {
  inherits(x, "rater_fit")
}

get_model <- function(f) {
  f$model
}

#' Get the posterior samples from a rater mcmc fit object
#'
#' @param fit A rater `mcmc_fit` object.
#'
#' @noRd
#'
get_samples <- function(fit) {
  fit$samples
}

get_estimates <- function(f) {
  f$estimates
}


# bit of a hack reusing get_data - should it be generic?
