#' Extract posterior samples from a rater fit object
#'
#' @param fit A rater fit object.
#' @param pars A character vector of parameter names to return. By default
#'   `c("pi", "theta")`.
#'
#' @return A named list of the posterior samples for each parameters. For each
#'   parameter the samples are in the form returned by [rstan::extract()].
#'
#' @details Posterior samples can only be returned for models fitting using
#'   MCMC not optimisation. In addition, posterior samples cannot be returned
#'   for the latent class due to the marginalisation technique used internally.
#'
#' @importFrom rstan extract
#'
#' @examples
#'
#' \dontrun{
#' fit <- rater(anesthesia, "dawid_skene")
#'
#' samples <- posterior_samples(fit)
#'
#' # Look at first 6 samples for each of the pi parameters
#' head(samples$pi)
#'
#' # Look at the first 6 samples for the theta[1, 1, 1] parameter
#' head(samples$theta[, 1, 1, 1])
#'
#' # Only get the samples for the pi parameter:
#' pi_samples <- posterior_samples(fit, pars = "pi")
#'
#' }
#'
#' @export
#'
posterior_samples <- function(fit, pars = c("pi", "theta")) {
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
#' @param object A rater `mcmc_fit` object.
#' @param prob A single probability. The size of the credible interval
#'   returned. By default `0.9`.
#' @param pars The parameters to calculate the intervals for
#' @param ... Other arguments.
#'
#' @return A matrix with 2 columns. The first column is the lower bound of
#'   of the credible interval and the second is the upper bound. Each row
#'   corresponds to one individuals parameters. The rownames are the parameter
#'   names.
#'
#' @details Posterior intervals can only be calculated for models fit with
#'   MCMC. In addition, posterior intervals are not meaningful for the latent
#'   class (and indeed cannot be calculated). The *full* posterior distribution
#'   of the latent class can be extracted using [class_probabilities]
#'
#' @examples
#'
#' \dontrun{
#' fit <- rater(anesthesia, "dawid_skene", verbose = FALSE, chains = 1)
#'
#' intervals <- posterior_interval(fit)
#' head(intervals)
#'
#' }
#'
#' @aliases posterior_interval
#' @method posterior_interval mcmc_fit
#' @importFrom rstantools posterior_interval
#' @export
#' @export posterior_interval
#'
posterior_interval.mcmc_fit <- function(object,
                                        prob = 0.9,
                                        pars = c("pi", "theta"),
                                        ...) {

  fit <- object
  # We could keep the stan data after fitting, but it doesn't seem worth
  # the added complexity.
  K <- fit$stan_data$K
  J <- fit$stan_data$J

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
#' @param object A rater optim_fit object
#' @param prob A probability
#' @param pars The parameters to calculate the intervals for
#' @param ... Other arguments
#'
#' @method posterior_interval optim_fit
#' @importFrom rstantools posterior_interval
#' @export
#' @export posterior_interval
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
#' @return A named list of the parameter estimates.
#'
#' @seealso `class_probabilities()`
#'
#' @examples
#'
#' \dontrun{
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
  out <- par[grep("pi", names(par))]
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
#' \dontrun{
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

#' Retrieve MCMC convergence diagnostics for a rater fit
#'
#' @param fit An rater `mcmc_fit` object.
#' @param pars A character vector of parameter names to return. By default
#'   `c("pi", "theta")`.
#'
#' @return A matrix where the columns represent different diagnostics and the
#'   rows are different parameters. Currently the first column contains
#'   the Rhat statistic and the second bulk effective samples size. The
#'   rownames contain the parameter names.
#'
#' @details MCMC diagnostics cannot be calculate for the z due to the
#'   marginalisation used to fit the models.
#'
#' @seealso [rstan::Rhat()], [rstan::ess_bulk()].
#'
#' @importFrom rstan extract Rhat ess_bulk
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @examples
#' \dontrun{
#'
#' fit <- rater(anesthesia, "dawid_skene")
#'
#' # Calculate the diagnostics for all parameters.
#' mcmc_diagnostics(fit)
#'
#' # Calculate the diagnostics just for the pi parameter.
#' mcmc_diagnostics(fit, pars = "pi")
#'
#' }
#'
#' @export
#'
mcmc_diagnostics <- function(fit, pars = c("pi", "theta")) {

  if (inherits(fit, "optim_fit")) {
    stop("Cannot extract MCMC diagnositcs from a optimisation fit.",
         call. = FALSE)
  }

  if ("z" %in% pars) {
    stop("Cannot extract MCMC diagnostics for the latent class.",
         call. = FALSE)
  }

  diagnostics <- matrix(nrow = 0, ncol = 2)

  if ("pi" %in% pars) {
    K <- fit$stan_data$K
    pi_diagnostics <- matrix(nrow = K, ncol = 2)
    row_names <- character(K)
    draws <- rstan::extract(get_samples(fit), pars = "pi", permuted = FALSE)
    for (i in 1:K) {
      name <- paste0("pi[", i, "]")
      pi_diagnostics[i, 1] <- rstan::Rhat(draws[, , name])
      pi_diagnostics[i, 2] <- rstan::ess_bulk(draws[, , name])
      row_names[[i]] <- name
    }
    colnames(pi_diagnostics) <- c("Rhat", "ess_bulk")
    rownames(pi_diagnostics) <- row_names

    diagnostics <- rbind(diagnostics, pi_diagnostics)
  }

  if ("theta" %in% pars) {
    K <- fit$stan_data$K
    J <- fit$stan_data$J
    theta_diagnostics <- matrix(nrow = J * K * K, ncol = 2)
    row_names <- character(J * K * K)
    draws <- rstan::extract(get_samples(fit), pars = "theta", permuted = FALSE)
    n <- 1
    for (j in 1:J) {
      for (k in 1:K) {
        for (i in 1:K) {
          stan_name <- sprintf("theta[%s,%s,%s]", j, k, i)
          theta_diagnostics[n, 1] <- rstan::Rhat(draws[, , stan_name])
          theta_diagnostics[n, 2] <- rstan::ess_bulk(draws[, , stan_name])
          row_names[[n]] <- sprintf("theta[%s, %s, %s]", j, k, i)
          n <- n + 1
        }
      }
    }
    colnames(theta_diagnostics) <- c("Rhat", "ess_bulk")
    rownames(theta_diagnostics) <- row_names

    diagnostics <- rbind(diagnostics, theta_diagnostics)
  }

  diagnostics
}
