#' Numerically stable log_sum_exp function
#'
#' @param x vector of real numbers
#'
#' @noRd
#'
logsumexp <- function(x) {
    y <- max(x)
    y + log(sum(exp(x - y)))
  }

#' Softmax function
#'
#' @param x vector of real numbers
#'
#' @noRd
#'
softmax <- function(x) {
  exp(x - logsumexp(x))
}

pi_to_long_format <- function(par) {
  K <- length(par)
  out <- matrix(par, ncol = 1, nrow = K)
  rownames(out) <- sprintf("pi[%s]", 1:K)
  out
}

theta_to_long_format <- function(par) {
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
  out <- matrix(values, nrow = J * K * K, ncol = 1)
  rownames(out) <- names
  out
}

z_to_long_format <- function(par) {
  I <- length(par)
  out <- matrix(par, ncol = 1, nrow = I)
  rownames(out) <- sprintf("z[%s]", 1:I)
  out
}
