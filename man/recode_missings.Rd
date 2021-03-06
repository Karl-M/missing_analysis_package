% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/recode_missings.R
\name{recode_missings}
\alias{recode_missings}
\title{Recode Values in Missings}
\usage{
recode_missings(data, a)
}
\arguments{
\item{data}{Object, can be a vector, a matrix, a dataframe or more general an array of arbitrarily high dimension}

\item{a}{Value(s) which should be recoded to NA}
}
\description{
Function for easy Recoding of values in vector or data.frame to NA
}
\examples{
#' to recode all columns in a data.frame

x <- c(2, 4, 1, -2, -1, 10)
y <- c(-2, -1, 3, 2, 9, -1)
z <- c(-1, -3, 4, 12, 3, 2)
df <- data.frame(x,y, z)
df.miss <- recode_missings(data = df, a = c(1,2))
}
\keyword{missings,}
\keyword{recode}

