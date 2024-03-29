% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.R
\name{mapping}
\alias{mapping}
\title{Generate a Mapping Function}
\usage{
mapping(from, to, na = NA, ch.as.fact = TRUE)
}
\arguments{
\item{from}{A vector.  This is the domain of the function.}

\item{to}{A vector of the same length as \code{from}. If omitted, then the
\code{names} of \code{from} are taken as the domain, and the values as the
values to map to. If \code{from} has no \code{names}, then \code{to} is equal to
\code{from} (useful for re-ordering \code{factor} levels).}

\item{na}{An alternative way to specify the value that \code{NA} maps to.
Ignored if from contains \code{NA}.}

\item{ch.as.fact}{A logical.  Should the mapping return a \code{factor}
instead of \code{character}?}
}
\value{
A function that translates from \code{from} to \code{to}.  The function also
has an \code{\link{inverse}} which is a function that performs the inverse mapping.
}
\description{
This function returns a function that does a simple mapping from one set of value to another.
It is a function-generating function.
}
\details{
This function returns a function.  When called with a vector
argument \code{x}, this function will return a vector \code{y} of
the same length as \code{x} and such that each element \code{y[i]}
is equal to \code{to[j]} where \code{j} is the smallest integer such
that \code{from[j] == x[i]}, and \code{NA} if no such \code{j}
exists.

Note: \code{from} will always be matched as a string, even if it is numeric.
So, \code{mapping(1, "A")} and \code{mapping("1", "A")} are the same, and
both functions will return \code{"A"} when called with either \code{1} or
\code{"1"}.
}
\examples{

sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
sex.mapping(c("Female", "Female", "Male", "F"))

sex.mapping <- mapping(0:1, c("Female", "Male"), na="Unknown")
sex.mapping(c(0, 1, NA, 0, 1, 1, 0))
inverse(sex.mapping)(c("Female", "Male", "Unknown"))

from <- c(0, 1, NA)
to <- c(NA, "Male", "Female")
x <- c(0, 1, NA, 0, 1, 1, 0)
sex.mapping <- mapping(c(0, 1, NA), c(NA, "Male", "Female"))
sex.mapping
sex.mapping(c(0, 1, NA, 0, 1, 1, 0))
inverse(sex.mapping)
inverse(sex.mapping)(c("Female", "Male", NA))

race.mapping <- mapping(c(
      "1"="WHITE",
      "2"="BLACK OR AFRICAN AMERICAN",
      "5"="AMERICAN INDIAN OR ALASKA NATIVE"))
race.mapping(1:5)

}
\seealso{
\code{\link[=inverse]{inverse()}},
\code{\link[=codomain]{codomain()}},
\code{\link[=domain]{domain()}},
\code{\link[=remap]{remap()}},
\code{\link[=text2mapping]{text2mapping()}},
\code{\link[=cut_mapping]{cut_mapping()}}
}
