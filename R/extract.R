#' Extract posterior draws from a rater_fit object
#'
#' @param fit A rater_fit object
#' @param pars A specification of which parameters to return draws from
#' @param ... Extra arguments
#'
#' @return If multiple draws are requested then a list of arrays, otherwise
#'  a single array.
#'
#' @importFrom rstan extract
#' @export
#'
posterior_draws <- function(fit, pars = c("pi", "theta"), ...) {
  if (inherits(fit, "optim_fit")) {
    stop("Cannot return draws from an optimisaton fit", call. = FALSE)
  }

  draws <- list()
  for (i in seq_along(pars)) {
    par <- match.arg(pars[[i]], c("pi", "theta", "z"))
    draws[[i]] <- switch(par,
      "pi"    = rstan::extract(fit$draws)$pi,
      "theta" = rstan::extract(fit$draws)$theta,
      "z"     = stop("Cannot return draws for marginalised discrete parameter",
                 call. = FALSE),
      stop("Invalid pars argument", call. = FALSE)
    )
  }

  if (length(draws) == 1L) {
    out <- draws[[1]]
  } else {
    out <- draws
  }
  out
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
  K <- fit$data$stan_data$K
  J <- fit$data$stan_data$J

  intervals <- list()
  for (i in 1:length(pars)) {
    par <- match.arg(pars[[i]], c("pi", "theta"))

    if (par == "pi") {
      pi_draws <- posterior_draws(fit, pars = "pi")
      colnames(pi_draws) <- sprintf("pi[%s]", 1:K)
      pi_interval <- rstantools::posterior_interval(pi_draws, prob, ...)
      intervals[[i]] <- pi_interval

    } else if (par == "theta") {
      theta_draws_raw <- posterior_draws(fit, pars = "theta")
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
    } else if (par == "pi") {
      stop("Cannot calculate quantiles for pi.", call. = FALSE)
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
  stop("Can't calculate posterior intervals for a model fit with",
       "optimisation.",
       call. = FALSE)
}

#' Extract point estimates of parameters from a fit object
#'
#' @param fit A rater_fit object
#' @param pars A character vector of parmeter names to select
#' @param format TODO
#'
#' @details If the passed fit object was fit using MCMC then the posterior
#'   means are used. If it was fit through optimisation the MAP esimates
#'   are returned.
#'
#' @return TODO
#'
point_estimate <- function(fit,
                           pars = c("pi", "theta", "z"),
                           ...) {
  out <- list()
  for (i in seq_along(pars)) {
    par <- pars[[i]]
    out[[i]] <- switch(par,
      "pi" = pi_point_estimate(fit, ...),
      "theta" = theta_point_estimate(fit, ...),
      "z" = z_point_estimate(fit, ...),
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

pi_point_estimate.mcmc_fit <- function(fit, ...) {
  pi_draws <- posterior_draws(fit, pars = "pi")
  apply(pi_draws, 2, mean)
}

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
#' @return Probalistic latent class measurements
#'
#' @export
#'
z_point_estimate <- function(fit, ...) {
  UseMethod("z_point_estimate")
}

z_point_estimate.mcmc_fit <- function(fit, ...) {
  # We can't use posterior_draws here because these are not technically draws.
  log_p_z_samps <- rstan::extract(fit$draws)$log_p_z
  p_z_samps <- aperm(apply(log_p_z_samps, c(1, 2), softmax), c(2, 3, 1))
  p_z <- apply(p_z_samps, c(2, 3), mean)
  if (is.table_data(fit$data)) {
    p_z <- enlarge_z(p_z, fit)
  }
  p_z
}

z_point_estimate.optim_fit <- function(fit, ...) {
  par <- fit$estimates$par
  stan_data <- fit$data$stan_data
  log_p_z_values <- par[grep("log_p_z", names(par))]
  log_p_z <- matrix(log_p_z_values, nrow = stan_data$I, ncol = stan_data$K)
  p_z <- t(apply(log_p_z, 1, softmax))
  if (is.table_data(fit$data)) {
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

theta_point_estimate.mcmc_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "hierarchical_dawid_skene" = theta_point_estimate_hds(),
    "dawid_skene" = theta_point_estimate_ds_mcmc(fit, which, ...),
    "class_conditional_dawid_skene" =
      theta_point_estimate_ccds_mcmc(fit, which, ...),
    stop("Model type not supported", call. = FALSE))
}

theta_point_estimate_ds_mcmc <- function(fit, which, ...) {
  theta_samps <- posterior_draws(fit, pars = "theta")

  J <- dim(theta_samps)[[2]]
  if (is.null(which)) {
    which <- 1:J
  }
  validate_which(which, J)

  theta <- apply(theta_samps, c(2, 3, 4), mean)
  theta[which, , ]
}

theta_point_estimate_ccds_mcmc <- function(fit, which, ...) {
  cc_theta_samps <- posterior_draws(fit, pars = "theta")

  J <- dim(cc_theta_samps)[[2]]
  if (is.null(which)) {
    which <- 1:J
  }
  validate_which(which, J)

  cc_theta <- apply(cc_theta_samps, c(2, 3), mean)
  theta <- unspool_cc_theta(cc_theta)
  theta[which, , ]
}

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
  K <- fit$data$stan_data$K
  J <- fit$data$stan_data$J
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
  stopifnot(is.table_data(fit$data))
  p_z[rep(1:nrow(p_z), fit$data$stan_data$tally), ]
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
