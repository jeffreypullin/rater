#' Extract latent class estimates from a fit
#'
#' @param fit fit object
#' @param ... extra args
#'
#' @return Probalistic latent class measurements
#'
#' @export
#'
extract_z.mcmc_fit <- function(fit, ...) {
  log_p_z_samps <- rstan::extract(fit$draws)$log_p_z
  log_p_z <- apply(log_p_z_samps, c(2, 3), mean)
  # not 100% sure why the transpose is needed here...
  p_z <- t(apply(log_p_z, 1, softmax))
  p_z  <- if (is.table_data(fit$data)) enlarge_z(p_z, fit) else p_z
  p_z
}

#' Extract prevalance information
#'
#' Extract prevalence/pi estiamtes from a fit object
#'
#' @param fit fit object
#' @param ... extra args
#'
#' @export
#'
extract_pi.mcmc_fit <- function(fit, ...) {
  apply(rstan::extract(fit$draws)$pi, 2, mean)
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
extract_theta.mcmc_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "multinomial" = extract_theta_m_mcmc(fit, which, ...),
    "hierarchical_dawid_skene" = extract_theta_hds(),
    "dawid_skene" = extract_theta_ds_mcmc(fit, which, ...),
    stop("Model type not supported", call. = FALSE))
}

extract_theta_ds_mcmc <- function(fit, which, ...) {
  theta_samps <- rstan::extract(fit$draws)$theta
  # I wonder if there is a better way to deal with the J issue...
  J <- dim(theta_samps)[[2]]
  which <- if (is.null(which)) 1:J else which
  validate_which(which, J)
  theta <- apply(theta_samps, c(2,3,4), mean)
  theta[which, , ]
}

extract_theta_m_mcmc <- function(fit, which, ...) {
  if (!is.null(which)) {
    warning("`which` arguement will be ignored (multinomial model)", call. = FALSE)
  }
  theta_samps <- rstan::extract(fit$draws)$theta
  colMeans(theta_samps)
}

extract_theta_hds <- function() {
  stop("Rater metrics cannot be extracted from the Hierachical Dawid and Skene model.",
       call. = FALSE)
}

# Methods for optim_fit

#' Extract latent class estimates from a optim fit
#'
#' @param fit fit object
#' @param ... extra args
#'
#' @return Probalistic latent class measurements
#'
#' @export
#'
extract_z.optim_fit <- function(fit, ...) {
  par <- fit$estimates$par
  stan_data <- fit$data$stan_data
  log_p_z_values <- par[grep("log_p_z", names(par))]
  log_p_z <- matrix(log_p_z_values, nrow = stan_data$I, ncol = stan_data$K)
  p_z <- t(apply(log_p_z, 1, softmax))
  # name?
  p_z  <- if (is.table_data(fit$data)) enlarge_z(p_z, fit) else p_z
  p_z
}

#' Extract prevalance information from optim fit object
#'
#' Extract prevalence/pi estiamtes from a fit object
#'
#' @param fit fit object
#' @param ... extra args
#'
#' @export
#'
extract_pi.optim_fit <- function(fit, ...) {
 par <- fit$estimates$par
 out <- par[grep("pi", names(par))]
 names(out) <- NULL
 out
}

#' Extract theta parameter for the optim_fit class
#'
#' @param fit fit object
#' @param which which raters to extract
#' @param ... extra args
#'
#' @return array of rater error distibutions
#'
#' @export
#'
extract_theta.optim_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "multinomial" = extract_theta_m_optim(fit, which, ...),
    "hierarchical_dawid_skene" = extract_theta_hds(),
    "dawid_skene" = extract_theta_ds_optim(fit, which, ...),
    stop("Model type not supported", call. = FALSE))
}

extract_theta_ds_optim <- function(fit, which, ...) {
  par <- fit$estimates$par
  theta_values <- par[grep("\\btheta\\b", names(par))]
  K <- fit$data$stan_data$K
  J <- fit$data$stan_data$J
  which <- if (is.null(which)) 1:J else which
  theta <- array(theta_values, dim = c(J, K, K))
  theta[which, , ]
}

extract_theta_m_optim <- function(fit, which, ...) {
  if (!is.null(which)) {
    warning("`which` arguement will be ignored (multinomial model)", call. = FALSE)
  }
  par <- fit$estimates$par
  theta_values <- par[grep("\\btheta\\b", names(par))]
  K <- fit$data$stan_data$K
  matrix(theta_values, nrow = K, ncol = K)
}

# helpers

validate_which <- function(which, J) {
  if (!(length(which) > 0) || !is.numeric(which)) {
    stop("which must be a positive length numeric vector", call. = FALSE)
  }
  # TODO make more informative
  if (length(which(which %in% 1:J)) != length(which)) {
    stop("All numbers in `which` must be drawn from 1:", J, call. = FALSE)
  }
}

enlarge_z <- function(p_z, fit) {
  # this will only be run if the data is in table data form
  stopifnot(is.table_data(fit$data))
  p_z[rep(1:nrow(p_z), fit$data$stan_data$tally), ]
}
