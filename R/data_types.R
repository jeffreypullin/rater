#' @name data_types
#'
#' @title Data types compatible with the models of categorical annotation
#' @description Functions to coerce/parse various types of input data
#'   for use in \code{\link{mcmc}()}.
#'
#' @return a \code{rater_data} object that can be passed to \code{\link{mcmc}}.
#'
NULL

# User friendly/visible functions

#' @rdname data_types
#' @export
#'
#' @param data a numeric matrix
#'
wide_data <- function(data) {
  if (!is.numeric(data) || class(data) != "matrix") {
    stop("Data must be a numeric matrix", call. = FALSE)
  }
  new_wide_data(data)
}

#' @rdname data_types
#' @export
#'
long_data <- function(data) {
  if (!is.numeric(data) || !(length(dim(data)) == 2) || ncol(data) != 3) {
    stop("Data must be a numeric matrix with three columns", call. = FALSE)
  }
  new_long_data(data)
}

#' @rdname data_types
#' @export
#'
multinomial_data <- function(data) {
  if (!is.numeric(data) || length(dim(data)) != 2 || ncol(data) != 2) {
    stop("Data must be a numeric matrix with two columns", call. = FALSE)
  }
  new_multinomial_data(data)
}

#' Internal helper to creat a wide_data object
#' @param data a numeric matrix
#'
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

#' Internal helper to creat a wide_data object
#' @param data a numeric matrix with three columns
#'
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

#' Internal helper to creat a wide_data object
#' @param data a numeric matrix with two columns
#'
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

# we could write validators for these - in terms of the validity of their stan_data?

# TODO Implment table data function



