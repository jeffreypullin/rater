#' Extract posterior draws from a rater_fit object
#'
#' @param fit A rater_fit object
#' @param pars A specification of which parameters to return draws from
#' @param ... Extra arguments
#'
#' @return A named list of parameter draws
#'
#' @importFrom rstan extract
#'
#' @export
#'
posterior_samples <- function(fit, pars = c("pi", "theta"), ...) {
  if (inherits(fit, "optim_fit")) {
    stop("Cannot return draws from an optimisaton fit", call. = FALSE)
  }

  samples <- list()
  for (par in pars) {
    par <- match.arg(par, c("pi", "theta", "z"))
    samples <- switch(par,
      "pi"    = c(samples, pi = list(rstan::extract(get_samples(fit))$pi)),
      "theta" = c(samples, theta = list(rstan::extract(get_samples(fit))$theta)),
      "z"     = stop("Cannot return draws for marginalised discrete parameter",
                     call. = FALSE),
      stop("Invalid pars argument", call. = FALSE)
    )
  }

  samples
}

#' Extract posterior intervals for parameters of the model
#'
#' @param object A rater mcmc_fit object
#' @param prob A probability
#' @param pars The parameters to calculate the intervals for
#'
#' @importFrom rstantools posterior_interval
#'
#' @export
posterior_interval.mcmc_fit <- function(object,
                                        prob = 0.9,
                                        pars = c("pi", "theta"),
                                        ...) {

  fit <- object
  # We could keep the stan data after fitting, but it doesn't seem worth
  # the added complexity.
  K <- max(fit$data$rating)
  J <- max(fit$data$rater)

  intervals <- list()
  for (i in 1:length(pars)) {
    par <- match.arg(pars[[i]], c("pi", "theta", "z"))

    if (par == "pi") {
      pi_draws <- posterior_samples(fit, pars = "pi")[[1]]
      colnames(pi_draws) <- sprintf("pi[%s]", 1:K)
      pi_interval <- rstantools::posterior_interval(pi_draws, prob, ...)
      intervals[[i]] <- pi_interval

    } else if (par == "theta") {
      theta_draws_raw <- posterior_samples(fit, pars = "theta")[[1]]
      n_draws <- dim(theta_draws_raw)[[1]]

      theta_draws_mat <- matrix(0, nrow = n_draws, ncol = J * K * K)
      col_names <- character(J * K * K)
      combs <- expand.grid(1:J, 1:K, 1:K)
      for (n in 1:nrow(combs)) {
        j <- combs[n, 1]
        k <- combs[n, 2]
        k_prime <- combs[n, 3]
        theta_draws_mat[, n] <- theta_draws_raw[, j, k, k_prime]
        col_names[[n]] <- sprintf("theta[%s, %s, %s]", j, k, k_prime)
      }
      colnames(theta_draws_mat) <- col_names
      intervals[[i]] <- rstantools::posterior_interval(theta_draws_mat,
                                                       prob, ...)
    } else if (par == "z") {
      stop("Cannot calculate quantiles for z", call. = FALSE)
    }
  }

  do.call(rbind, intervals)
}

#' Extract posterior intervals for parameters of the model
#'
#' @param object TODO
#' @param prob TODO
#'
#' @importFrom rstantools posterior_interval
#'
#' @export
#'
posterior_interval.optim_fit <- function(object,
                                         prob = 0.9,
                                         pars = c("pi", "theta"),
                                         ...) {
  stop("Can't calculate posterior intervals for a model fit using",
       " optimisation.",
       call. = FALSE)
}

#' Extract point estimates of parameters from a fit object
#'
#' @param fit A rater_fit object
#' @param pars A character vector of parmeter names to select
#' @param ... Extra arguments
#'
#' @details If the passed fit object was fit using MCMC then the posterior
#'   means are used. If it was fit through optimisation the MAP esimates
#'   are returned. The z parameter returned is which value of the class
#'   probabilities is largest.
#'
#' @return A named list of the parameter values. See details for the precise
#'   statistical interpretation of the values.
#'
#' @examples
#' fit <- rater(anesthesia, dawid_skene(), method = "optim")
#' point_estimate(fit, pars = "pi")
#'
#' @export
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

#' Extract a point esimate of the pi parameter from an MCMC fit
#'
#' @param fit A rater_fit object
#'
#' @return A vector of length K containing the posterior mean of pi
#'
#' @export
#'
pi_point_estimate <- function(fit, ...) {
  UseMethod("pi_point_estimate")
}

#' @rdname pi_point_estimate
#' @export
pi_point_estimate.mcmc_fit <- function(fit, ...) {
  pi_draws <- posterior_samples(fit, pars = "pi")[[1]]
  apply(pi_draws, 2, mean)
}

#' @rdname pi_point_estimate
#' @export
pi_point_estimate.optim_fit <- function(fit, ...) {
  par <- fit$estimates$par
  out <- par[grep("pi", names(par))]
  names(out) <- NULL
  out
}

#' Extract latent class estimates from a fit
#'
#' @param fit fit object
#' @param ... extra args
#'
#' @details This function returns actual estimates of the latent class i.e.
#'   whole numbers from 1 to K. These are found by taking the class with the
#'   highest probability.
#'
#' @return Latent class estimates: A vector length I consisting of whole
#'   numbers from 1 to K.
#'
#' @export
#'
z_point_estimate <- function(fit, ...) {
  p_z <- class_probabilities(fit, ...)
  # which.max only takes the first maximum if multiple are found but this
  # is not a problem as we are dealing with floats.
  apply(p_z, 1, which.max)
}

#' Extract latent class probabilites from an object.
#'
#' @param fit A rater fit object.
#' @param ... Extra arguments.
#'
#' @return A I * K matrix where each element is the probabily of item i being
#'   of class k.
#'
#' @export
#'
class_probabilities <- function(fit, ...) {
  UseMethod("class_probabilities")
}

#' @rdname class_probabilities
#' @export
class_probabilities.mcmc_fit <- function(fit, ...) {
  # We can't use posterior_samples here because these are not technically draws.
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
  I <- max(fit$data$item)
  K <- max(fit$data$rating)
  log_p_z_values <- par[grep("log_p_z", names(par))]
  log_p_z <- matrix(log_p_z_values, nrow = I, ncol = K)
  p_z <- t(apply(log_p_z, 1, softmax))
  if (fit$data_format == "grouped") {
    enlarge_z(p_z, fit)
  }
  p_z
}

#' Extract rater accuracy estimates for the Dawid Skene models
#'
#' Extract rater accuracy/theta estimates from a Dawid Skene fit object
#'
#' @param fit fit object
#' @param which which raters to extract
#' @param ... extra args
#'
#' @return list of matrices containing probability confusion matrices for each
#'  rater
#'
#' @export
#'
theta_point_estimate <- function(fit, which = NULL, ...) {
  UseMethod("theta_point_estimate")
}

#' @rdname theta_point_estimate
#' @export
theta_point_estimate.mcmc_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "hierarchical_dawid_skene" = theta_point_estimate_hds(),
    "dawid_skene" = theta_point_estimate_ds_mcmc(fit, which, ...),
    "class_conditional_dawid_skene" =
      theta_point_estimate_ccds_mcmc(fit, which, ...),
    stop("Model type not supported", call. = FALSE))
}

theta_point_estimate_ds_mcmc <- function(fit, which, ...) {
  theta_samps <- posterior_samples(fit, pars = "theta")[[1]]

  J <- dim(theta_samps)[[2]]
  if (is.null(which)) {
    which <- 1:J
  }
  validate_which(which, J)

  theta <- apply(theta_samps, c(2, 3, 4), mean)
  theta[which, , ]
}

theta_point_estimate_ccds_mcmc <- function(fit, which, ...) {
  cc_theta_samps <- posterior_samples(fit, pars = "theta")[[1]]

  J <- dim(cc_theta_samps)[[2]]
  if (is.null(which)) {
    which <- 1:J
  }
  validate_which(which, J)

  cc_theta <- apply(cc_theta_samps, c(2, 3), mean)
  theta <- unspool_cc_theta(cc_theta)
  theta[which, , ]
}

#' @rdname theta_point_estimate
#' @export
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
  K <- max(fit$data$rating)
  J <- max(fit$data$rater)
  if (is.null(which)) {
    which <- 1:J
  }
  theta <- array(theta_values, dim = c(J, K, K))
  theta[which, , ]
}

theta_point_estimate_ccds_optim <- function(fit, which, ...) {
  par <- fit$estimates$par
  cc_theta_values <- par[grep("\\btheta\\b", names(par))]
  K <- fit$data$stan_data$K
  J <- fit$data$stan_data$J
  if (is.null(which)) {
    which <- 1:J
  }
  cc_theta <- matrix(cc_theta_values, nrow = J, ncol = K)
  theta <- unspool_cc_theta(cc_theta)
  theta[which, , ]
}

theta_point_estimate_hds <- function() {
  stop("Rater metrics cannot be extracted from the Hierachical Dawid and
       Skene model.", call. = FALSE)
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
  p_z[rep(1:nrow(p_z), fit$data$n), ]
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
