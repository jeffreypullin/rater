% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rater_fit_class.R
\name{plot.rater_fit}
\alias{plot.rater_fit}
\title{Plot a \code{rater_fit} object}
\usage{
\method{plot}{rater_fit}(
  x,
  pars = "theta",
  prob = 0.9,
  rater_index = NULL,
  item_index = NULL,
  theta_plot_type = "matrix",
  ...
)
}
\arguments{
\item{x}{An object of class \code{rater_fit}.}

\item{pars}{A length one character vector specifying the parameter to plot.
By default \code{"theta"}.}

\item{prob}{The coverage of the credible intervals shown in the \code{"pi"} plot.
If not plotting pi this argument will be ignored. By default \code{0.9}.}

\item{rater_index}{The indexes of the raters shown in the \verb{"theta} plot.
If not plotting theta this argument will be ignored. By default \code{NULL}
which means that all raters will be plotted.}

\item{item_index}{The indexes of the items shown in the class probabilities
plot. If not plotting the class probabilities this argument will be
ignored. By default \code{NULL} which means that all items will be plotted.
This argument is particularly useful to focus the subset of items with
substantial uncertainty in their class assignments.}

\item{theta_plot_type}{The type of plot of the "theta" parameter. Can be
either \code{"matrix"} or \code{"points"}. If \code{"matrix"} (the default) the plot
will show the point estimates of the individual rater error matrices,
visualised as tile plots. If \code{"points"}, the elements of the theta
parameter will be displayed as points, with associated credible intervals.
Overall, the \code{"matrix"} type is likely more intuitive, but the \code{"points"}
type can also visualise the uncertainty in the parameter estimates.}

\item{...}{Other arguments.}
}
\value{
A ggplot2 object.
}
\description{
Plot a \code{rater_fit} object
}
\details{
The use of \code{pars} to refer to only one parameter is for backwards
compatibility and consistency with the rest of the interface.
}
\examples{

\donttest{
fit <- rater(anesthesia, "dawid_skene")

# By default will just plot the theta plot
plot(fit)

# Select which parameter to plot.
plot(fit, pars = "pi")

# Plot the theta parameter for rater 1, showing uncertainty.
plot(fit, pars = "theta", theta_plot_type = "points", rater_index = 1)

}

}
