% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-hasNonzeroRowsAndCols.R
\name{check-scalar-hasNonzeroRowsAndCols}
\alias{check-scalar-hasNonzeroRowsAndCols}
\alias{hasNonzeroRowsAndCols}
\title{Does the input contain non-zero rows and columns?}
\usage{
hasNonzeroRowsAndCols(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Useful for quickly checking to see if we have dropped rows or columns
containing all zeros.
}
\details{
This is a common check when handling RNA-seq data prior to generating a
heatmap or applying a log transformation, for example.
}
\note{
Updated 2021-01-04.
}
\examples{
## TRUE ====
x <- matrix(data = seq_len(4L), nrow = 2L)
print(x)
hasNonzeroRowsAndCols(x)

## FALSE ====
x <- matrix(data = rep(c(0L, 1L), times = 2L), nrow = 2L, byrow = FALSE)
print(x)
hasNonzeroRowsAndCols(x)

x <- matrix(data = rep(c(0L, 1L), times = 2L), nrow = 2L, byrow = TRUE)
print(x)
hasNonzeroRowsAndCols(x)
}
