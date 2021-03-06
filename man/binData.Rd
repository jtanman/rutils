% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_functions.R
\name{binData}
\alias{binData}
\title{Create Bins for Specific Columns in a Data Frame}
\usage{
binData(data, columns, num_quantiles, include.lowest = FALSE, right = TRUE,
  dig.lab = 3)
}
\arguments{
\item{data}{data frame with the data and columns in it}

\item{columns}{names of the input columns which are to be binned}

\item{num_quantiles}{numeric or vector of numerics, represent the number of
quantiles to split all or each of the input columns by order into (will not
be the final number of bins since many will be identical)}

\item{include.lowest}{logical, indicating if an ‘x[i]’ equal to the lowest
(or highest, for right = FALSE) ‘breaks’ value should be included.}

\item{right}{logical, indicating if the intervals should be closed on the
right (and open on the left) or vice versa.}

\item{dig.lab}{integer which is used when labels are not given. It determines
the number of digits used in formatting the break numbers.}
}
\value{
\code{binColumn} returns a list with the following components:

\item{data}{data frame with the bins as additional columns}

\item{num_bins}{vector of the number of bins for each column}
}
\description{
\code{binData} returns a data frame with the bins as extra columns and
calculates the number of bins created for each column. The bins are
created by quantiles on the data with the quantiles specified by num_quantiles and
returned as a factor.
}
\note{
This function can be called on its own but it's mainly meant to be
}
