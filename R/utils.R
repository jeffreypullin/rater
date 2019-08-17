#' Numerically stable log_sum_exp function
#' @param x vector of real numbers
logsumexp <- function (x) {
    y <- max(x)
    y + log(sum(exp(x - y)))
  }

#' Softmax function
#' @param x vector of real numbers
softmax <- function (x) {
  exp(x - logsumexp(x))
}

#' Check hat there has been no divergent transitions or poor convergence
#' @param draws stanfit object
#' @importFrom rstan summary
check_convergence <- function(draws) {
  # divergent transitions are displayed automatically be stan
  rhats <- rstan::summary(draws)$summary[, "Rhat"]
  if (sum(rhats > 1.1)) {
    warning("Some R-hat statistics were above 1.1. The model fit may be invalid",
            call. = FALSE)
  }
}

plot_names <- c("theta", "raters", "pi", "prevalence", "z", "latent_class")
