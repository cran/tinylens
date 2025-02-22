% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe-lenses.R
\name{indices_l}
\alias{indices_l}
\alias{i_l}
\title{Subset lens}
\usage{
indices_l(...)

i_l(...)
}
\arguments{
\item{...}{Conditions to subset by. Unnamed arguments are used as indices.
Named arguments are passed along to \code{[} for viewing and are removed for
setting.}
}
\value{
A lens that subsets the object by the specified indices
}
\description{
This function returns a lens that subsets the object in a generalized way.
}
\examples{
d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
l <- indices_l(1, 1)
# get the first row of first column
view(d, l)
# set the first row of first column
set(d, l, 1)

# get the first row
l <- indices_l(1,)
view(d, l)
# set the first row
set(d, l, 1)
}
