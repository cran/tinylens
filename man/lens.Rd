% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lens.R
\name{lens}
\alias{lens}
\title{Create a lens}
\usage{
lens(view, set = NULL)
}
\arguments{
\item{view}{A function that takes an object and returns a value}

\item{set}{A function that takes an object and a value and returns a new object}
}
\value{
A lens with the specified view and set functions
}
\description{
A lens is a pair of functions that can be used to view and set a value in an object.
Lenses are implemented as S7 classes.
}
\details{
A "proper" lens should satisfy the following so-called "lens laws":
\itemize{
\item \strong{View-Set}: \code{set(d, l, view(d, l)) == d}
\item \strong{Set-View}: \code{view(set(d, l, x), l) == x}
\item \strong{Set-Set}: \code{set(set(d, l, x), l, y) == set(d, l, y)}
}

These laws are not enforced by \code{tinylens}, but you should strive to follow them
when creating your own lenses.

A best effort has been made to ensure that these laws hold for the lenses
provided by \code{tinylens}, but this is trickier than it might seem because of
how R handles subset assignments.
}
\examples{
# create a trivial identity lens
l <- lens(view = function(x) x, set = function(x, value) value)
}
