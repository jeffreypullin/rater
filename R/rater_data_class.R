# is beautify/annotation?

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

is.rater_data <- function(x) {
  inherits(x, "rater_data")
}
