#' Returns a character vector of the geoms of a ggplot2 plot
#'
#' Taken from: https://bit.ly/2B7TzEw (shortened)
#'
#' @param p a ggplot2 plot
get_geoms <- function(p) {
  sapply(p$layers, function(x) class(x$geom)[1])
}

#' Returns the number of facets of a ggplot2 plot
#'
#' Taken from: https://bit.ly/2UriIB4 (shortened)
#'
#' @param p a ggplot2 plot
get_facet_dim <- function(p) {
  length(unique(ggplot2::ggplot_build(p)$data[[1]]$PANEL))
}

# Taken from greta.
expect_ok <- function(expr) {
  expect_error(expr, NA)
}



