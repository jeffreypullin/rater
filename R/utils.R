# Helper function
# Could also dispatch on class - does

get_name <- function(model){
  if (class(model)[[1]] == "dawid_skene") {
    "Bayesian Dawid and Skene Model"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}

# May not be necceasary
# could just call it the name
get_file <- function(model) {
  if (class(model)[[1]] == "dawid_skene") {
    "dawid_skene"
  } else {
    stop("Model type not supported", call. = FALSE)
  }
}
