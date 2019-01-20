#' Extract latent class estimates from a fit
#'
#' @param rateR fit object
#' @return Probalistic latent class measurements
#'
#' @export
extract_latent_class <- function(fit) {

  validate_fit(fit)

  log_p_z_samps <- rstan::extract(fit$draws)$log_p_z

  I <- dim(log_p_z_samps)[[2]]
  K <- dim(log_p_z_samps)[[3]]


  # TODO probably should mean and then softmax...
  log_p_z <- matrix(0, nrow = I, ncol = K)
  for(i in 1:I){
    for (k in 1:K){
      log_p_z[i,k] <- mean(log_p_z_samps[, i, k])
    }
  }

  # apply softmax
  p_z <- matrix(0, nrow = I, ncol = K)
  for (i in 1:I){
    p_z[i, ] <- softmax(log_p_z[i, ])
  }

  p_z
}

#' Extract prevalance information
#'
#' Extract prevalence/pi estiamtes from a fit object
#'
#' @param fit fit object
#' @return data.frame of estimated mean category probaility and standard deviation
#'
#' @export
extract_prevalance <- function(fit) {

  validate_fit(fit)

  pi_samps <- rstan::extract(fit$draws)$pi

  pi_mean <- apply(pi_samps, 2, mean)
  pi_sd   <- apply(pi_samps, 2, sd)

  out <- data.frame(category = 1:ncol(pi_samps),
                    prob = pi_mean,
                    sd = pi_sd)

  out

}
