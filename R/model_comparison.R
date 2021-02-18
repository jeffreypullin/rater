#' Compute the PSIS LOO CV - a measure of model fit - of a rater fit object.
#'
#' @param x A `rater_fit` object. All model types are currently supported
#'   except the basic Dawid-Skene model fit with grouped data.
#' @param ... Other arguments passed.
#' @param cores The number of cores to use when calling the underlying
#'   functions. By default the value of the `mc.cores` option.
#'
#' @return A loo object.
#'
#' @details This function is somewhat experimental; model comparison is always
#'   difficult and choosing between variants of the Dawid-Skene model should
#'   be largely guided by considerations of data size and what is known about
#'   the characteristics of the raters. loo is, however, one of the leading
#'   methods for Bayesian model comparison and should provide a helpful guide
#'   in many situations.
#'
#'   When calculating loo we always use the relative effective
#'   sample size, calculated using `loo::relaive_eff` to improve the estimates
#'   of the PSIS effective sample sizes and Monte Carlo error.
#'
#'   For further information about the details of loo and PSIS please consult
#'   the provided references.
#'
#' @examples
#'
#' \donttest{
#' fit_ds <- rater(anesthesia, "dawid_skene", verbose = FALSE, chains = 1)
#' fit_ccds <- rater(anesthesia, "class_conditional_dawid_skene",
#'                   verbose = FALSE, chains = 1)
#'
#' loo_ds <- loo(fit_ds)
#' loo_ccds <- loo(fit_ccds)
#'
#' # To compare the loos easily we can use the loo_compare function from the
#' # loo package:
#' library(loo)
#'
#' loo_compare(loo_ds, loo_ccds)
#'
#' # The documentation of the loo package contains more information about how
#' # the output should be interpreted.
#' }
#'
#' @references
#' Vehtari, A., Gelman, A., and Gabry, J. (2017a). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC.
#' *Statistics and Computing*. 27(5), 1413--1432. doi:10.1007/s11222-016-9696-4
#' ([journal version](https://link.springer.com/article/10.1007/s11222-016-9696-4),
#'  [preprint arXiv:1507.04544](https://arxiv.org/abs/1507.04544)).
#'
#' Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2019).
#' Pareto smoothed importance sampling.
#' [preprint arXiv:1507.02646](https://arxiv.org/abs/1507.02646)
#'
#' @importFrom loo extract_log_lik relative_eff loo
#'
#' @aliases loo
#' @method loo rater_fit
#' @importFrom loo loo
#' @export
#' @export loo
#'
loo.rater_fit <- function(x,
                          ...,
                          cores = getOption("mc.cores", 1)) {

  if (x$data_format == "grouped") {
    stop("loo is not supported for models fit using grouped data.",
         call. = FALSE)
  }

  if (inherits(x, "optim_fit")) {
    stop("loo cannot be calculated for models fit using optimisation.",
         acall. = FALSE)
  }

  log_lik <- loo::extract_log_lik(x$samples, merge_chains = FALSE)
  r_eff <- loo::relative_eff(exp(log_lik), cores = cores)
  loo <- loo::loo(log_lik, r_eff = r_eff, cores = cores)

  loo
}
