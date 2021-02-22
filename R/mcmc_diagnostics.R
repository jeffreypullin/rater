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
#'   These MCMC diagnostics are intended as basic sanity check of the quality
#'   of the MCMC samples returned. Users who want more in depth diagnostics
#'   should consider using [as_mcmc.list()] to convert the samples to a
#'   [coda::mcmc.list()] object, or [get_stanfit()] to extract the underlying
#'   stanfit object.
#'
#' @seealso [rstan::Rhat()], [rstan::ess_bulk()] [as_mcmc.list()],
#'   [get_stanfit()].
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
#' \donttest{
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

  if (is.hier_dawid_skene(get_model(fit)) && ("theta" %in% pars)) {
     stop("theta cannot be extracted from the Hierachical Dawid-Skene model.",
          "\nConsider using `pars = c('pi')`.", call. = FALSE)
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
    draws <- rstan::extract(get_samples(fit), pars = "theta", permuted = FALSE)

    theta_diagnostics <- matrix(nrow = J * K * K, ncol = 2)
    row_names <- character(J * K * K)

    if (inherits(get_model(fit), "class_conditional_dawid_skene")) {
      n <- 1
      for (j in 1:J) {
        for (k in 1:K) {
          par_draws <- draws[, , sprintf("theta[%s,%s]", j, k)]
          for (i in 1:K) {
            theta_diagnostics[n, 1] <- rstan::Rhat(par_draws)
            theta_diagnostics[n, 2] <- rstan::ess_bulk(par_draws)
            row_names[[n]] <- sprintf("theta[%s, %s, %i]", j, k, i)
            n <- n + 1
          }
        }
      }
    } else {
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
    }
    colnames(theta_diagnostics) <- c("Rhat", "ess_bulk")
    rownames(theta_diagnostics) <- row_names

    diagnostics <- rbind(diagnostics, theta_diagnostics)
  }

  diagnostics
}
