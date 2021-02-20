#' Convert wide data to the long format
#'
#' @param data Data in a wide format. Must be 2D data object which can be
#'   converted to a data.frame
#'
#' @return The data converted into long format. A data.frame with three columns
#'   item, rater and rating.
#'
#' @details Wide data refers to a way of laying out categorical rating data
#'   where each item is one row and each column represents the ratings of each
#'   rater. Elements of the data can be `NA`, indicating that an item wasn't
#'   rated by a rater. Wide data cannot represent the same rater rating an item
#'   multiple times.
#'
#'   Currently any column names of the data are ignored and the raters are
#'   labelled by their column position (1 indexed, left to right). Only numeric
#'   ratings are currently supported.
#'
#' @examples
#' wide_data <- data.frame(dater_1 = c(3, 2, 2), rater_2 = c(4, 2, 2))
#' wide_data
#'
#' long_data <- wide_to_long(wide_data)
#' long_data
#'
#'
#' @export
#'
wide_to_long <- function(data) {

  if (!inherits(data, "data.frame") &&  !inherits(data, "matrix")) {
    stop("`data` must be a data.frame or matrix.", call = FALSE)
  }
  data <- as.data.frame(data)

  # FIXME We should accept non-numeric data (GitHub issue: #81) but for
  # now we explicitly check that is all columns contain numeric values.
  if (!all(vapply(data, is.numeric, FUN.VALUE = logical(1)))) {
    stop("All columns in `data` must contain only numeric values.",
         call. = FALSE)
  }

  values <- unlist(unclass(data))
  non_na_values <- values[!is.na(values)]
  if (any(non_na_values == 0)) {
    stop("Some ratings are 0. All ratings must be in 1:K",
         " where K is the number of classes.",
         call. = FALSE)
  }

  len <- length(non_na_values)

  rating <- numeric(len)
  rater <- numeric(len)
  item <- numeric(len)

  n <- 1
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (!is.na(data[[i, j]])) {

       rating[[n]] <- data[i, j]
       item[[n]] <- i
       rater[[n]] <- j

       n <- n + 1
      }
    }
  }

  data.frame(item = item, rater = rater, rating = rating)
}


