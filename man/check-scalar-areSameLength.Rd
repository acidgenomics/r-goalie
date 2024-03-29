% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-areSameLength.R
\name{check-scalar-areSameLength}
\alias{check-scalar-areSameLength}
\alias{areSameLength}
\title{Do the inputs have the same length?}
\usage{
areSameLength(x, y)
}
\arguments{
\item{x}{Object.}

\item{y}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Do the inputs have the same length?
}
\note{
Non-zero lengths for \code{x} and \code{y} are required, otherwise the check
function will intentionally error.

Updated 2023-09-29.
}
\examples{
## TRUE ====
x <- list(a = 1L, b = 2L)
y <- list(c = 3L, d = 4L)
areSameLength(x = x, y = y)

## FALSE ====
x <- list(a = 1L)
y <- list(b = 2L, c = 3L)
areSameLength(x = x, y = y)
}
