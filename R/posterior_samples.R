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
#'   For the class conditional model the 'full' theta parameterisation (i.e.
#'   appearing to have the same number of parameters as the standard
#'   Dawid-Skene model) is calculated and returned. This is designed to allow
#'   easier comparison with the full Dawid-Skene model.
#'
#' @importFrom rstan extract
#'
#' @examples
#'
#' \donttest{
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
      "theta" = {
        if (inherits(fit$model, "hier_dawid_skene")) {
           stop("theta cannot be extracted from the Hierachical Dawid-Skene model.",
                "\nConsider using `pars = c('pi')`.", call. = FALSE)
        }

        raw_theta <- rstan::extract(get_samples(fit))$theta
        if (inherits(fit$model, "class_conditional_dawid_skene")) {
          N <- dim(raw_theta)[[1]]
          J <- fit$stan_data$J
          K <- fit$stan_data$K
          full_theta <- array(dim = c(N, J, K, K))
          for (i in seq_len(N)) {
            full_theta[i, , , ] <- unspool_cc_theta(raw_theta[i, , ])
          }
        } else {
          full_theta <- raw_theta
        }
        c(samples, theta = list(full_theta))
      },
      "z"     = stop("Cannot return draws for marginalised discrete parameter",
                     call. = FALSE),
      stop("Invalid pars argument", call. = FALSE)
    )
  }

  samples
}
