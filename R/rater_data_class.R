# is beautify/annotation a possibiliy?

print.wide_data <- function(x, ...) {
  cat("`wide` rater data\n")
  print(get_data(x))
}

print.long_data <- function(x, ...) {
  cat("`long` rater data\n")
  print(get_data(x))
}

print.multinomial_data <- function(x, ...) {
  cat("`multinomial` rater data\n")
  print(get_data(x))
}

summary.rater_data <- function(object, ...) {
  cat("rater data")
}

`[.rater_data` <- function(x, i, j, drop = TRUE) {
  x$data[i, j, drop = drop]
}

`[[.rater_data` <- function(x, i, j, drop = TRUE) {
  stop("Please use `[` to subset rater_data", call. = FALSE)
}

dim.rater_data <- function(x) {
  dim(get_data(x))
}

as.matrix.rater_data <- function(x, ...) {
  get_data(x)
}

is.rater_data <- function(x) {
  inherits(x, "rater_data")
}



