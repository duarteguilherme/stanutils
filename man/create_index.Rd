% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle-data.R
\name{create_index}
\alias{create_index}
\title{Create numeric index for stan}
\usage{
create_index(data, ...)
}
\arguments{
\item{data}{tibble or data.frame object}

\item{...}{columns to be transformed to index}
}
\description{
This function creates numeric index for variables
inside a tibble
}
\examples{
df <- data.frame(test = c(rep("dog", 5), rep("cat", 6)))
create_index(df, k = test)
}
