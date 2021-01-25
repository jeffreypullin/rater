#' Extract point estimates of parameters from a fit object
#'
#' @param fit A rater fit object
#' @param pars A character vector of parameter names to return. By default
#'   `c("pi", "theta", "z")`.
#' @param ... Extra arguments
#'
#' @details If the passed fit object was fit using MCMC then the posterior
#'   means are returned. If it was fit through optimisation the maximum a
#'   priori (MAP) estimates are returned. The z parameter returned is the
#'   value of class probabilities which is largest. To return the full
#'   posterior distributions of the latent class use `class_probabilities()`.
#'
#'   For the class conditional model the 'full' theta parameterisation (i.e.
#'   appearing to have the same number of parameters as the standard
#'   Dawid-Skene model) is calculated and returned. This is designed to allow
#'   easier comparison with the full Dawid-Skene model.
#'
#' @return A named list of the parameter estimates.
#'
#' @seealso `class_probabilities()`
#'
#' @examples
#'
#' \donttest{
#' # A model fit using MCMC.
#' mcmc_fit <- rater(anesthesia, "dawid_skene")
#'
#' # This will return the posterior mean (except for z)
#' post_mean_estimate <- point_estimate(mcmc_fit)
#'
#' # A model fit using optimisation.
#' optim_fit <- rater(anesthesia, dawid_skene(), method = "optim")
#'
#' # This will output MAP estimates of the parameters.
#' map_estimate <- point_estimate(optim_fit)
#'
#' }
#'
#' @export
#'
point_estimate <- function(fit,
                           pars = c("pi", "theta", "z"),
                           ...) {
  out <- list()
  for (par in pars) {
    out <- switch(par,
      "pi" = c(out, pi = list(pi_point_estimate(fit, ...))),
      "theta" = c(out, theta = list(theta_point_estimate(fit, ...))),
      "z" = c(out, z = list(z_point_estimate(fit, ...))),
      stop("Unknown parameter passed", call. = FALSE)
    )
  }

  out
}

#' Extract a point estimate of the pi parameter from an MCMC fit
#'
#' @param fit A rater object.
#' @param ... Other arguments.
#'
#' @return A vector of length K containing the posterior mean (`mcmc_fit`)
#'   or MAP estimate (`optim_fit`) of pi.
#'
#' @noRd
pi_point_estimate <- function(fit, ...) {
  UseMethod("pi_point_estimate")
}

#' @rdname pi_point_estimate
#' @noRd
pi_point_estimate.mcmc_fit <- function(fit, ...) {
  pi_draws <- posterior_samples(fit, pars = "pi")[[1]]
  apply(pi_draws, 2, mean)
}

#' @rdname pi_point_estimate
#' @noRd
pi_point_estimate.optim_fit <- function(fit, ...) {
  par <- fit$estimates$par
  # ^ is an anchor for the start of the line - needed due to the log_pi in the
  # HDS model.
  out <- par[grep("^pi", names(par))]
  names(out) <- NULL
  out
}

#' Extract latent class estimates from a fit
#'
#' @param fit A rater fit object.
#' @param ... Extra arguments.
#'
#' @details This function returns actual estimates of the latent class i.e.
#'   whole numbers from 1 to K. This is taken to be the latent class with the
#'   highest probability. (This can be thought of a kind of post-hoc MAP
#'   estimate.)
#'
#' @return Latent class estimates: A vector length I consisting of whole
#'   numbers from 1 to K.
#'
#' @noRd
#'
z_point_estimate <- function(fit, ...) {
  p_z <- class_probabilities(fit, ...)
  # which.max only takes the first maximum if multiple are found but this
  # is not a problem as we are dealing with floats.
  apply(p_z, 1, which.max)
}

#' Extract latent class probabilities from a rater fit object
#'
#' @param fit A rater fit object.
#' @param ... Extra arguments.
#'
#' @return A I * K matrix where each element is the probably of item i being
#'   of class k. (I is the number of items and K the number of classes).
#'
#' @details The latent class probabilities are obtained by marginalising out
#'   the latent class and then calculating, for each draw of pi and theta, the
#'   conditional probability of the latent class given the other parameters
#'   and the data. Averaging these conditional probabilities gives the
#'   (unconditional) latent class probabilities retuned by this function.
#'
#' @examples
#' \donttest{
#'
#' fit <- rater(anesthesia, "dawid_skene")
#' class_probabilities(fit)
#'
#' }
#'
#' @export
#'
class_probabilities <- function(fit, ...) {
  UseMethod("class_probabilities")
}

#' @rdname class_probabilities
#' @export
class_probabilities.mcmc_fit <- function(fit, ...) {
  # We can't use posterior_samples here because these are not technically
  # draws.
  log_p_z_samps <- rstan::extract(get_samples(fit))$log_p_z
  p_z_samps <- apply(log_p_z_samps, c(1, 2), softmax)
  p_z_samps <- aperm(p_z_samps, c(2, 3, 1))
  p_z <- apply(p_z_samps, c(2, 3), mean)
  if (fit$data_format == "grouped") {
    p_z <- enlarge_z(p_z, fit)
  }
  p_z
}

#' @rdname class_probabilities
#' @export
class_probabilities.optim_fit <- function(fit, ...) {
  par <- fit$estimates$par
  K <- fit$stan_data$K
  if (fit$data_format == "grouped") {
    I <- length(fit$stan_data$tally)
  } else {
    I <- fit$stan_data$I
  }
  log_p_z_values <- par[grep("log_p_z", names(par))]
  log_p_z <- matrix(log_p_z_values, nrow = I, ncol = K)
  p_z <- t(apply(log_p_z, 1, softmax))
  if (fit$data_format == "grouped") {
    p_z <- enlarge_z(p_z, fit)
  }
  p_z
}

#' Extract rater accuracy estimates for the Dawid-Skene models
#'
#' Extract rater accuracy/theta estimates from a Dawid Skene fit object
#'
#' @param fit A rater fit object.
#' @param which Which rater's error matrices should be returned.
#' @param ... Extra arguments.
#'
#' @return An array of K * K matrices each a rater's accuracy matrix.
#'
#' @noRd
#'
theta_point_estimate <- function(fit, which = NULL, ...) {
  UseMethod("theta_point_estimate")
}

#' @rdname theta_point_estimate
#' @noRd
theta_point_estimate.mcmc_fit <- function(fit, which = NULL, ...) {

  if (inherits(fit$model, "hier_dawid_skene")) {
    theta_point_estimate_hds()
  }

  # We now 'unspool' the theta parameter for the class conditional model by
  # default so this works for both the standard and class conditional models.
  theta_samps <- posterior_samples(fit, pars = "theta")[[1]]

  J <- dim(theta_samps)[[2]]
  if (is.null(which)) {
    which <- 1:J
  }
  validate_which(which, J)

  theta <- apply(theta_samps, c(2, 3, 4), mean)
  theta[which, , ]
}

#' @rdname theta_point_estimate
#' @noRd
theta_point_estimate.optim_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "hierarchical_dawid_skene" = theta_point_estimate_hds(),
    "dawid_skene" = theta_point_estimate_ds_optim(fit, which, ...),
    "class_conditional_dawid_skene" =
      theta_point_estimate_ccds_optim(fit, which, ...),
    stop("Model type not supported", call. = FALSE))
}

theta_point_estimate_ds_optim <- function(fit, which, ...) {
  par <- fit$estimates$par
  theta_values <- par[grep("\\btheta\\b", names(par))]
  K <- fit$stan_data$K
  J <- fit$stan_data$J
  if (is.null(which)) {
    which <- 1:J
  }
  theta <- array(theta_values, dim = c(J, K, K))
  theta[which, , ]
}

theta_point_estimate_ccds_optim <- function(fit, which, ...) {
  par <- fit$estimates$par
  cc_theta_values <- par[grep("\\btheta\\b", names(par))]
  K <- fit$stan_data$K
  J <- fit$stan_data$J
  if (is.null(which)) {
    which <- 1:J
  }
  cc_theta <- matrix(cc_theta_values, nrow = J, ncol = K)
  theta <- unspool_cc_theta(cc_theta)
  theta[which, , ]
}

theta_point_estimate_hds <- function() {
  stop("theta cannot be extracted from the Hierachical Dawid-Skene model.",
       "\nConsider using `pars = c('pi', 'z')`.", call. = FALSE)
}

# Helper functions

validate_which <- function(which, J) {
  if (!(length(which) > 0) || !is.numeric(which)) {
    stop("which must be a positive length numeric vector", call. = FALSE)
  }
  # TODO Make this error more informative.
  if (length(which(which %in% 1:J)) != length(which)) {
    stop("All numbers in `which` must be drawn from 1:", J, call. = FALSE)
  }
}

enlarge_z <- function(p_z, fit) {
  stopifnot(fit$data_format == "grouped")
  p_z[rep(1:nrow(p_z), fit$stan_data$tally), ]
}

unspool_cc_theta <- function(cc_theta) {
  J <- nrow(cc_theta)
  K <- ncol(cc_theta)
  theta_out <- array(0, dim = c(J, K, K))
  for (j in 1:J) {
    for (k in 1:K) {
      theta_out[j, k, ] <- (1 - cc_theta[j, k]) / (K - 1)
      theta_out[j, k, k] <- cc_theta[j, k]
    }
  }
  theta_out
}
