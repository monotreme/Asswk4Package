% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot fars accident data}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A length one vector containing integer, or string which is convertable to an integer}

\item{year}{A length one vector containg integer, or string which is convertable to an integer
If the input parameter is not convertable to a string, a warning message will be produced.
The warning message for an input of "foo" reads: 'In make_filename("foo") : NAs introduced by coercion'

The function imports the following packages:
                                             dplyr::filter
                                             maps::map
                                             graphics::points}
}
\value{
NULL
For an input parameter of "1994", the returned string is "accident_1994.csv.bz2". For a non-integer input value of,
for example, "foo", the returned string is ""accident_NA.csv.bz2"
}
\description{
This function takes an integer corresponding to a state state and a year, and plots the data from the corresponding fars file.
}
\examples{
fars_map_state(1,c(2013))
fars_map_state("1",2015)
fars_map_state(6,"2014")

}

