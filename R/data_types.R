
# we could write validators for these - in terms of the validity of their stan_data
# can we just have one validator?

# should/can we use the wrapper around new_ pattern?

# User friendly functions

wide_data <- function(data) {
  if (!is.numeric(data) || class(data) != "matrix") {
    stop("Data must be a numeric matrix")
  }
  new_wide_data(data)
}

long_data <- function(data) {
  if (!is.numeric(data) || !(length(dim(data)) == 2) || ncol(data) != 3) {
    stop("Data must be a numeric matrix with three columns")
  }
  new_long_data(data)
}

multinomial_data <- function(data) {
  if (!is.numeric(data) || length(dim(data)) != 2 || ncol(data) != 2) {
    stop("Data must be a numeric matrix with two columns")
  }
  new_multinomial_data(data)
}

# Constructors

new_wide_data <- function(data) {
  I <- nrow(data)
  J <- ncol(data)
  K <- max(data)
  mask <- !is.na(data)
  ii <- rep(1:I, each = I)[mask]
  jj <- rep(1:J, J)[mask]
  y <- as.vector(data)[mask]
  N <- length(y)
  stan_data <- list(N = N, I = I, J = J, K = K, ii = ii, jj = jj, y = y)
  d <- list(data = data, stan_data = stan_data)
  class(d) <- c("wide_data", "rater_data")
  d
}

new_long_data <- function(data) {
  stan_data <- list(N = nrow(data),
                    I = max(data[, 1]),
                    J = max(data[, 2]),
                    K = max(data[, 3]),
                    ii = data[, 1],
                    jj = data[, 2],
                    y = data[, 3])
  d <- list(data = data, stan_data = stan_data)
  class(d) <- c("long_data", "rater_data")
  d
}

new_multinomial_data <- function(data) {
  stan_data <- list(N = nrow(data),
                    K = max(data[, 2]),
                    I = max(data[, 1]),
                    ii = data[, 1],
                    y = data[, 2])
  d <- list(data = data, stan_data = stan_data)
  class(d) <- c("multinomial_data", "rater_data")
  d
}




# TODO
# table_data <- function(coding, N) {
#   # can't remmber the approapriate function
#   cat("in production")
# }



