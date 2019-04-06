# An S3 class for the data types used in this package

#' @export
print.wide_data <- function(x, ...) {
  cat("`wide` rater data\n")
  print(get_data(x))
}

#' @export
print.long_data <- function(x, ...) {
  cat("`long` rater data\n")
  print(get_data(x))
}

#' @export
print.multinomial_data <- function(x, ...) {
  cat("`multinomial` rater data\n")
  print(get_data(x))
}

#' @export
print.table_data <- function(x, ...) {
  cat("`table` rater data\n")
  print(get_data(x))
}

#' @export
summary.rater_data <- function(object, ...) {
  cat("rater data")
}

#' @export
`[.rater_data` <- function(x, i, j, drop = TRUE) {
  x$data[i, j, drop = drop]
}

#' @export
`[[.rater_data` <- function(x, i, j, drop = TRUE) {
  stop("Please use `[` to subset rater_data", call. = FALSE)
}

#' @export
dim.rater_data <- function(x) {
  dim(get_data(x))
}

#' @export
as.matrix.rater_data <- function(x, ...) {
  get_data(x)
}

is.rater_data <- function(x) {
  inherits(x, "rater_data")
}

get_data <- function(d) {
  d$data
}

get_stan_data <- function(d) {
  d$stan_data
}


