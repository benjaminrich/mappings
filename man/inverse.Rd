% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.R
\name{inverse}
\alias{inverse}
\title{Inverse of a mapping}
\usage{
inverse(x)
}
\arguments{
\item{x}{A \code{\link{mapping}}.}
}
\value{
The inverse \code{\link{mapping}}.
}
\description{
Given a \code{\link{mapping}} \code{x}, return the inverse mapping.
}
\examples{
sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
sex.inverse.mapping <- inverse(sex.mapping)
sex.inverse.mapping(c(0, 0, 1, 0))
}
