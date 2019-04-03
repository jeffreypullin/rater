#' Extract latent class estimates from a fit
#'
#' @param fit fit object
#' @return Probalistic latent class measurements
#'
#' @export
#'
extract_z.mcmc_fit <- function(fit, ...) {
  log_p_z_samps <- rstan::extract(fit$draws)$log_p_z
  log_p_z <- apply(log_p_z_samps, c(2, 3), mean)
  # not 100% sure why the transpose is needed here...
  p_z <- t(apply(log_p_z, 1, softmax))
  p_z
}

#' Extract prevalance information
#'
#' Extract prevalence/pi estiamtes from a fit object
#'
#' @param fit fit object
#'
#' @export
#'
extract_pi.mcmc_fit <- function(fit, ...) {
  apply(rstan::extract(fit$draws)$pi, 2, mean)
}

#' Extract rater accuarcy estimates for the Dawid Skene models
#'
#' Extract rater accuarcy/theta estimates from a Dawid Skene fit object
#'
#' @param fit fit object
#' @param which which raters to extract
#' @return list of matrices containing proabaility confusion matrices for each
#'  rater
#'
#' @export
#'
extract_theta.mcmc_fit <- function(fit, which = NULL, ...) {
  switch(fit$model$file,
    "multinomial" = extract_theta_m_mcmc(fit, which, ...),
    "hierarchical_dawid_skene" = extract_theta_hds_mcmc(),
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

extract_theta_hds_mcmc <- function() {
  stop("Rater metrics cannot be extracted from the Hierachical Dawid and Skene model.",
       call. = FALSE)
}

validate_which <- function(which, J) {
  if (!(length(which) > 0) || !is.numeric(which)) {
    stop("which must be a positive length numeric vector", call. = FALSE)
  }
  # TODO make more informative
  if (length(which(which %in% 1:J)) != length(which)) {
    stop("All numbers in `which` must be drawn from 1:", J, call. = FALSE)
  }
}
