
#' Gets the long name of a model
#' @param model object of type model
get_name <- function(model){
  if (is.dawid_skene(model)) {
    "Bayesian Dawid and Skene Model"
  } else if (is.hier_dawid_skene(model)) {
    "Bayesian Hierarchical Dawid and Skene Model"
  } else if (is.multinomial(model)) {
    "Bayesian Multinomial (Annotator pooled) Model"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}

#' Gets stan file name of a model
#' @param model object of type model
get_file <- function(model) {
  if (is.dawid_skene(model)) {
    "dawid_skene"
  } else if (is.hier_dawid_skene(model)){
    "hierarchical_dawid_skene"
  } else if (is.multinomial(model)){
    "multinomial"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
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



