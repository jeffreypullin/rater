#' Coerce from wide to long format
#'
#' Coerces data in a wide format (possibly with NA's) to a long format
#'
#' @param data in wide format
#' @return Data in long format
#'
#' @export
wide_to_long <- function(data) {

  # check if 2-D
  if (length(dim(data)) != 2) {
    stop("Data must be two dimensional!", call. = FALSE)
  }

  len <- sum(!is.na(data))

  y  <- numeric(len)
  jj <- numeric(len)
  ii <- numeric(len)

  n <- 1
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {

      # remove null values
      if (!is.na(data[i,j])) {

        # add the value in
        y[n] <- data[i, j]

        # add the item index
        ii[n] <- i

        # add the rater
        jj[n] <- j

        n <- n + 1
      }
    }
  }

  out <- data.frame(ii = ii, jj = jj, y = y)

  out
}
