# An S3 class for the data types used in this package

#' @s3method
print.wide_data <- function(x, ...) {
  cat("`wide` rater data\n")
  print(get_data(x))
}

#' @s3method
print.long_data <- function(x, ...) {
  cat("`long` rater data\n")
  print(get_data(x))
}

#' @s3method
print.multinomial_data <- function(x, ...) {
  cat("`multinomial` rater data\n")
  print(get_data(x))
}

#' @s3method
summary.rater_data <- function(object, ...) {
  cat("rater data")
}

#' @s3method
`[.rater_data` <- function(x, i, j, drop = TRUE) {
  x$data[i, j, drop = drop]
}

#' @s3method
`[[.rater_data` <- function(x, i, j, drop = TRUE) {
  stop("Please use `[` to subset rater_data", call. = FALSE)
}

#' @s3method
dim.rater_data <- function(x) {
  dim(get_data(x))
}

#' @s3method
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


