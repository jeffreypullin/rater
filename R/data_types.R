
# we could write validators for these - in terms of the validity of their stan_data
# can we just have one validator?

# should/can we use the wrapper around new_ pattern?


wide_data <- function(data) {
  if (!is.numeric(data) || class(data) != "matrix") {
    stop("Data must be a numeric matrix")
  }
  d <- list(data = data, stan_data = wide_to_stan(data))
  class(d) <- c("wide_data", "rater_data")
  d
}

long_data <- function(data) {
  if (!is.numeric(data) || !(length(dim(data)) == 2) || ncol(data) != 3) {
    stop("Data must be a numeric matrix with three columns")
  }
  d <- list(data = data, stan_data = long_to_stan(data))
  class(d) <- c("long_data", "rater_data")
  d
}

multinomial_data <- function(data) {
  if (!is.numeric(data) || length(dim(data)) != 2 || ncol(data) != 2) {
    stop("Data must be a numeric matrix with two columns")
  }
  d <- list(data = data, stan_data = multinomial_to_stan(data))
  class(d) <- c("multinomial_data", "rater_data")
  d
}


# TODO
# table_data <- function(coding, N) {
#   # can't remmber the approapriate function
#   cat("in production")
# }

# could also have tabled multinomial data....
# but the model for multinomial is simpler and should run faster + plus not
# core functioanllity


wide_to_stan <- function(data) {
  I <- nrow(data)
  J <- ncol(data)
  K <- max(data)
  mask <- !is.na(data)
  ii <- rep(1:I, each = I)[mask]
  jj <- rep(1:J, J)[mask]
  y <- as.vector(data)[mask]
  N <- length(y)
  list(N = N, I = I, J = J, K = K, ii = ii, jj = jj, y = y)
}

multinomial_to_stan <- function(data) {
  list(N = nrow(data),
       K = max(data[, 2]),
       I = max(data[, 1]),
       ii = data[, 1],
       y = data[, 2])
}

long_to_stan <- function(data) {
  list(N = nrow(data),
       I = max(data[, 1]),
       J = max(data[, 2]),
       K = max(data[, 3]),
       ii = data[, 1],
       jj = data[, 2],
       y = data[, 3])
}

# Make this generic
# validate_stan_data

# these validators arw aimed at the consistency of the stan_data
# TODO (BIG)
#validate_long_data <-



