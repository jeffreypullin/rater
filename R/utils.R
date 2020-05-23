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

# What an ugly function...
to_long_format <- function(par, par_name) {
  if (par_name == "pi") {
    K <- length(pi)
    par_out <- matrix(par, ncol = 1, nrow = K)
    rownames(par_out) <- sprintf("pi[%s]", 1:K)
  } else if (par_name == "theta") {
    J <- dim(par)[[1]]
    K <- dim(par)[[2]]
    n <- 1
    values <- numeric(J * K * K)
    names <- character(J * K * K)
    for (j in 1:J) {
      for (k in 1:K) {
        for (i in 1:K) {
          values[[n]] <- par[j, k, i]
          names[[n]] <- sprintf("theta[%s, %s, %s]", j, k, i)
          n <- n + 1
        }
      }
    }
    par_out <- matrix(values, nrow = J * K * K, ncol = 1)
    rownames(par_out) <- names
  } else if (par_name == "z") {
    I <- nrow(par)
    K <- ncol(par)
    print(I)
    print(K)
    values <- numeric(I * K)
    names <- character(I * K)
    n <- 1
    for (i in 1:I) {
      for (k in 1:K) {
        values[[n]] <- par[i, k]
        names[[n]] <- sprintf("z[%s, %s]", i, k)
        n <- n + 1
      }
    }
    par_out <- matrix(values, nrow = I * K, ncol = 1)
    rownames(par_out) <- names
  }
  par_out
}
