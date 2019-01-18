# Helper function
# Could also dispatch on class - does

get_name <- function(model){
  if (class(model)[[1]] == "dawid_skene") {
    "Bayesian Dawid and Skene Model"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}

# May not be necceasary
# could just call it the name
get_file <- function(model) {
  if (class(model)[[1]] == "dawid_skene") {
    "dawid_skene"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}

#' Numerically stable log_sum_exp function
logsumexp <- function (x) {
    y <- max(x)
    y + log(sum(exp(x - y)))
  }

#' Softmax function
softmax <- function (x) {
  exp(x - logsumexp(x))
}

#' Check that a passed object is actually of type fit
validate_fit <- function(fit) {
  if (all(class(fit) != "fit")) {
    stop("Cannot plot a non-fit object", call. = FALSE)
  }
}
