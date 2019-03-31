#' Gets the long name of a model
#'
#' @param m object of type rater_model
#'
get_name <- function(m) {
  m$name
}

#' Gets stan file name of a model
#'
#' @param m object of type rater_model
#'
get_file <- function(m) {
  m$file
}

get_parameters <- function(m) {
  m$parameters
}

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

#' Check that a passed object is actually of type fit
#' @param fit fit object
validate_fit <- function(fit) {
  if (all(class(fit) != "fit")) {
    stop("Cannot plot a non-fit object", call. = FALSE)
  }
}

#' Check hat there has been no divergent transitions or poor convergence
#' @param draws stanfit object
check_convergence <- function(draws) {

  if (sum(rstan::get_divergent_iterations(draws))) {
    warning("There were divergent transitions. The model fit may be invalid",
            call. = FALSE)
  }
  rhats <- rstan::summary(draws)$summary[, "Rhat"]
  if (sum(rhats > 1.1)) {
    warning("Some R-hat statistics were above 1.1. The model fit may be invalid",
            call. = FALSE)
  }

}

is.stanfit <- function(x) {
  # this is S4 syntax...
  is(x, "stanfit")
}
