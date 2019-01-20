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
#' @return data.frame of estimated mean category probaility and standard
#'   deviation
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


#' Extract rater accuarcy estimates
#'
#' Extract ater accuarcy/theta estimates from a fit object
#'
#' @param fit fit object
#' @return list of matrices containsing proabaility confusion matrices for each
#'  rater
#'
#' @export
extract_raters <- function(fit, which = NULL) {

  validate_fit(fit)


  fit_ss <- rstan::extract(fit$draws)
  theta_samps <- fit_ss$theta

  J <- dim(theta_samps)[2]
  K <- dim(theta_samps)[3]

  raters <- list()
  for(j in 1:J){
    rate_mat <- matrix(0, nrow = K, ncol = K)
    for (n in 1:K){
      for (m in 1:K){
        rate_mat[n,m] <- mean(theta_samps[,j,n,m])
      }
    }
    raters[[j]] <- rate_mat
  }

  names(raters) <- paste("rater", rep(1:J), sep = "_")

  if (is.null(which)) {
    which <- 1:J
  }

  # must validate after which has been converted from NULL (potentially)
  validate_which(which)

  out <- raters[which]

  out

}

validate_which <- function(which) {

  if (!(length(which) > 0 & is.numeric(which))) {
    stop("which must be a positve length numeric vector", call. = FALSE)
  }

}
