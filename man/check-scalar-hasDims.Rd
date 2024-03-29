% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-hasDims.R
\name{check-scalar-hasDims}
\alias{check-scalar-hasDims}
\alias{hasDims}
\alias{hasRows}
\alias{hasCols}
\title{Does the input have dimensions?}
\usage{
hasDims(x, n = NULL)

hasRows(x, n = NULL)

hasCols(x, n = NULL)
}
\arguments{
\item{x}{Object.}

\item{n}{\code{integer} or \code{NULL}.
Expected dimension number.
For \code{hasDims}, \code{integer(2)} is required, corresponding to rows, columns.
If \code{NULL}, only checks for non-zero dimensions.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Does the input have dimensions?
}
\note{
Updated 2023-10-06.
}
\examples{
## TRUE ====
x <- datasets::mtcars
hasDims(x)
hasRows(x)
hasCols(x)

## Note that dims don't have to be non-zero, just not NULL.
hasDims(data.frame())

## Expected dimension number is supported.
x <- matrix(data = seq(from = 1L, to = 6L), nrow = 3L, ncol = 2L)
## For `hasDims`, `n` corresponds to rows, columns.
hasDims(x, n = c(3L, 2L))
hasRows(x, n = 3L)
hasCols(x, n = 2L)

## FALSE ====
x <- data.frame()
hasDims(list())
hasRows(x)
hasCols(x)
}
\seealso{
\itemize{
\item \code{dim()} or \code{BiocGenerics::dims()} for \code{DFrameList}.
\item \code{nrow()} or \code{BiocGenerics::nrows()} for \code{DFrameList}.
\item \code{ncol()} or \code{BiocGenerics::ncols()} for \code{DFrameList}.
\item \code{assertive.properties::has_dims()}.
\item \code{assertive.properties::has_rows()}.
\item \code{assertive.properties::has_cols()}.
}
}
