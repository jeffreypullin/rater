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
#' @examples
#' \donttest{
#'
#' # Suppress sampling output.
#' mcmc_fit <- rater(anesthesia, "dawid_skene", verbose = FALSE)
#' print(mcmc_fit)
#'
#' }
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
#' @examples
#' \donttest{
#'
#' optim_fit <- rater(anesthesia, "dawid_skene", method = "optim")
#' print(optim_fit)
#'
#' }
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
#' @param pars A length one character vector specifying the parameter to plot.
#'   By default `"theta"`.
#' @param prob The coverage of the credible intervals shown in the `"pi"` plot.
#'   If not plotting pi this argument will be ignored. By default `0.9`.
#' @param rater_index The indexes of the raters shown in the `"theta` plot.
#'   If not plotting theta this argument will be ignored. By default `NULL`
#'   which means that all raters will be plotted.
#' @param item_index The indexes of the items shown in the class probabilities
#'   plot. If not plotting the class probabilities this argument will be
#'   ignored. By default `NULL` which means that all items will be plotted.
#'   This argument is particularly useful to focus the subset of items with
#'   substantial uncertiuanlty in their class assignments.
#' @param ... Other arguments.
#'
#' @return A ggplot2 object.
#'
#' @details The use of `pars` to refer to only one parameter is for backwards
#'  compatibility and consistency with the rest of the interface.
#'
#' @examples
#'
#' \donttest{
#' fit <- rater(anesthesia, "dawid_skene")
#'
#' # By default will just plot the theta plot
#' plot(fit)
#'
#' # Select which parameter to plot.
#' plot(fit, pars = "pi")
#'
#' }
#'
#' @export
#'
plot.rater_fit <- function(x,
                           pars = "theta",
                           prob = 0.9,
                           rater_index = NULL,
                           item_index = NULL,
                           ...) {

  if (length(pars) > 1 || !is.character(pars)) {
    stop("`pars` must be a length 1 character vector.", call. = FALSE)
  }

  which <- rater_index
  plot_names <- c("theta", "raters",
                  "pi", "prevalence",
                  "class_probabilities", "latent_class")

  par <- match.arg(pars, plot_names)

  if (is.hier_dawid_skene(get_model(x)) && (par %in% c("theta", "raters"))) {
    stop("Cannot plot rater error matrices - theta - for hierarchical model.",
         call. = FALSE)
  }

  plot <- switch(par,
    "theta" = plot_theta(x, which = which),
    "raters" = plot_theta(x, which = which),
    "class_probabilities" = plot_class_probabilities(x,
                                                     item_index = item_index),
    "latent_class" = plot_class_probabilities(x, item_index = item_index),
    # Luckily "p" will fall through correctly.
    "pi" = plot_pi(x, prob = prob),
    "prevalence" = plot_pi(x, prob = prob),
    "z" = stop("Cannot plot z directly.", call. = FALSE),
    stop("Invalid pars argument", call. = FALSE)
  )

  plot
}

#' Summarise a `mcmc_fit` object
#'
#' @param object An object of class `mcmc_fit`.
#' @param n_pars The number of pi/theta parameters and z 'items' to display.
#' @param ... Other arguments passed to function.
#'
#' @details For the class conditional model the 'full' theta parameterisation
#'   (i.e. appearing to have the same number of parameters as the standard
#'   Dawid-Skene model) is calculated and returned. This is designed to allow
#'   easier comparison with the full Dawid-Skene model.
#'
#' @examples
#' \donttest{
#'
#' fit <- rater(anesthesia, "dawid_skene", verbose = FALSE)
#'
#' summary(fit)
#'
#' }
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

  pars <- pi

  if (!inherits(get_model(fit), "hier_dawid_skene")) {
    # Prepare theta.
    theta_est <- theta_to_long_format(theta_point_estimate(fit))
    colnames(theta_est) <- "mean"
    theta_interval <- posterior_interval(fit, pars = "theta")
    theta_mcmc_diagnostics <- mcmc_diagnostics(fit, pars = "theta")
    theta <- cbind(theta_est, theta_interval, theta_mcmc_diagnostics)

    pars <- rbind(pi, theta)
  }

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
  # pars is a matrix where each row is a parametery thing.
  if (nrow(pars) > n_pars) {
    n_remaining <- nrow(pars) - n_pars
    cat("# ... with", n_remaining, "more rows\n")
  }

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
#' @details For the class conditional model the 'full' theta parameterisation
#'   (i.e. appearing to have the same number of parameters as the standard
#'   Dawid-Skene model) is calculated and returned. This is designed to allow
#'   easier comparison with the full Dawid-Skene model.
#'
#' @examples
#' \donttest{
#'
#' fit <- rater(anesthesia, "dawid_skene", method = "optim")
#'
#' summary(fit)
#'
#' }
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

  pars <- pi

  # Prepare theta.
  if (!inherits(get_model(fit), "hier_dawid_skene")) {
    theta <- theta_to_long_format(theta_point_estimate(fit))
    colnames(theta) <- "mean"
    pars <- rbind(pi, theta)
  }

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
  if (length(pars) > n_pars) {
    n_remaining <- length(pars) - n_pars
    cat("# ... with", n_remaining, "more rows\n")
  }

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

#' Provide a summary of the priors specified in a `rater_fit` object.
#'
#' @param object A `rater_fit` object.
#' @param ... Other arguments.
#'
#' @examples
#' \donttest{
#' # Fit a model using MCMC (the default).
#' fit <- rater(anesthesia, "dawid_skene", verbose = FALSE)
#'
#' # Summarise the priors (and model) specified in the fit.
#' prior_summary(fit)
#'
#' }
#'
#' @aliases prior_summary
#' @method prior_summary rater_fit
#' @importFrom rstantools prior_summary
#' @export
#' @export prior_summary
#'
prior_summary.rater_fit <- function(object, ...) {
  get_model(object)
}

#' Get the underlying `stanfit` object from a `rater_fit` object.
#'
#' @param fit A `rater_fit` object.
#'
#' @return A `stanfit` object from rstan.
#'
#' @examples
#'
#' \donttest{
#' fit <- rater(anesthesia, "dawid_skene", verbose = FALSE)
#'
#' stan_fit <- get_stanfit(fit)
#' stan_fit
#'
#' }
#'
#' @export
#'
get_stanfit <- function(fit) {

  if (!inherits(fit, "rater_fit")) {
    stop("`fit` must be rater_fit object.", call. = FALSE)
  }

  if (inherits(fit, "optim_fit")) {
    stan_fit <- fit$estimates
  } else {
    stan_fit <- fit$samples
  }

  stan_fit
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
