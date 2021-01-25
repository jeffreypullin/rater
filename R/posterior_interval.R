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
#'   For the class conditional model the 'full' theta parameterisation (i.e.
#'   appearing to have the same number of parameters as the standard
#'   Dawid-Skene model) is calculated and returned. This is designed to allow
#'   easier comparison with the full Dawid-Skene model.
#'
#' @examples
#'
#' \donttest{
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
      n <- 1
      for (j in 1:J) {
        for (k in 1:K) {
          for (i in 1:K) {
            theta_draws_mat[, n] <- theta_draws_raw[, j, k, i]
            col_names[[n]] <- sprintf("theta[%s, %s, %s]", j, k, i)
            n <- n + 1
          }
        }
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
