#' @name data_types
#'
#' @title Data types compatible with the models of categorical annotation
#' @description Functions to coerce/parse various types of input data
#'   for use in \code{\link{rater}()}.
#'
#' @return a \code{rater_data} object that can be passed to \code{\link{rater}}.
#'
NULL

# User friendly/visible functions

#' @rdname data_types
#' @export
#'
#' @param data a numeric matrix
#'
wide_data <- function(data) {
  if (!is.numeric(data) || !inherits(data, "matrix")) {
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
table_data <- function(data) {
  if (!is.numeric(data) || !inherits(data, "matrix")) {
    stop("Data must be a numeric matrix", call. = FALSE)
  }
  new_table_data(data)
}

#' Internal helper to creat a wide_data object
#' @param data a numeric matrix
#'
new_wide_data <- function(data) {
  I <- nrow(data)
  J <- ncol(data)
  K <- max(data)
  mask <- !is.na(data)
  ii <- rep(1:I, J)[mask]
  jj <- rep(1:J, each = I)[mask]
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

#' Internal helper to creat a table_data object
#' @param data a numeric matrix
#'
new_table_data <- function(data) {
  tally <- data[, ncol(data)]
  key <- data[, 1:(ncol(data) - 1)]
  stan_data <- list(N = nrow(data),
                    K = max(key),
                    J = ncol(key),
                    key = key,
                    tally = tally)
  d <- list(data = data, stan_data = stan_data)
  class(d) <- c("table_data", "rater_data")
  d
}

# TODO: write validators




