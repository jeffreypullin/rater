% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_format.R
\name{wide_to_long}
\alias{wide_to_long}
\title{Convert wide data to the long format}
\usage{
wide_to_long(data)
}
\arguments{
\item{data}{Data in a wide format. Must be 2D data object which can be
converted to a data.frame}
}
\value{
The data converted into long format. A data.frame with three columns
item, rater and rating.
}
\description{
Convert wide data to the long format
}
\details{
Wide data refers to a way of laying out categorical rating data
where each item is one row and each column represents the ratings of each
rater. Elements of the data can be \code{NA}, indicating that an item wasn't
rated by a rater. Wide data cannot represent the same rater rating an item
multiple times.

Currently any column names of the data are ignored and the raters are
labelled by their column position (1 indexed, left to right). Only numeric
ratings are currently supported.
}
\examples{
wide_data <- data.frame(dater_1 = c(3, 2, 2), rater_2 = c(4, 2, 2))
wide_data

long_data <- wide_to_long(wide_data)
long_data


}
