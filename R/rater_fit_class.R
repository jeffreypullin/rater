#' Make a mcmc_rater fit object
#'
#' @param model a rater model
#' @param samples a stanfit object
#' @param stan_data the data passed to Stan to fit the model
#' @param data_format The format of the data used to fit the model
#'
#' @return a rater fit object
#'
#' @noRd
new_mcmc_fit <- function(model, samples, stan_data, data_format) {
  new <- list(model = model,
              samples = samples,
              stan_data = stan_data,
              data_format = data_format)
  class(new) <- c("mcmc_fit", "rater_fit")
  new
}

#' Make a optim_rater fit object
#'
#' @param model a rater model
#' @param estimates a stan optimisation object
#' @param stan_data the data used to fit the model
#' @param data_format The format of the data used to fit the model
#'
#' @return a rater fit object
#'
#' @noRd
new_optim_fit <- function(model, estimates, stan_data, data_format) {
  new <- list(model = model,
              estimates = estimates,
              stan_data = stan_data,
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
  cat(get_name(get_model(x)), "with MCMC draws.\n")
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
  cat(get_name(get_model(x)), "with MAP estimates.\n")
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
      "pi" = plot_pi(x),
      "prevalence" = plot_pi(x),
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

#' Summary of mcmc fit
#'
#' @param object object of type rater fit
#' @param n_pars the number of pi/theta parameters and z 'items' to display
#' @param ... other args passed to function
#'
#' @method summary mcmc_fit
#'
#' @importFrom utils head
#'
#' @export
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
  n_remaining <- length(pars) - n_pars
  cat("# ... with", n_remaining, "more parameters\n")

  cat("\nz:\n")
  print(round(head(z_out, n_pars), 2))
  n_remaining_z <- nrow(z) - n_pars
  cat("# ... with", n_remaining_z, "more items\n")

}

#' Summary of optim fit
#'
#' @param object object of type rater fit
#' @param n_pars the number of pi/theta parameters and z 'items' to display
#' @param ... other args passed to function
#'
#' @method summary optim_fit
#'
#' @importFrom utils head
#'
#' @export
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

get_model <- function(f) {
  f$model
}

#' Get the posterior samples from a rater mcmc fit object
#'
#' @param fit A rater mcmc fit object
#'
#' @noRd
get_samples <- function(fit) {
  fit$samples
}

get_estimates <- function(f) {
  f$estimates
}


# bit of a hack reusing get_data - should it be generic?
