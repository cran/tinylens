% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe-lenses.R
\name{select_l}
\alias{select_l}
\title{include verbs.R
include lens.R
Select lens}
\usage{
select_l(...)
}
\arguments{
\item{...}{Columns to select}
}
\value{
A lens that selects the specified columns
}
\description{
This function returns a lens that selects the specified columns. Requires
\code{tidyselect} to be installed.
}
\examples{
d <- data.frame(x = 1:10, y = 11:20, z = 21:30)
l <- select_l(x, y)
# get the x and y columns
view(d, l)
# set the x and y columns
set(d, l, 1)
}
