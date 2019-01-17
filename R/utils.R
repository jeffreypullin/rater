# Helper function
# Could also dispatch on class

get_name <- function(model){
  if (class(model)[[1]] == "dawid_skene") {
    "Bayesian Dawid and Skene Model"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}

get_file <- function(model) {
  path <- "/Users/jeffreypullin/Documents/R/rateR/inst/stan"
  if (class(model)[[1]] == "dawid_skene") {
    file.path(path, "dawid_skene.stan")
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}
