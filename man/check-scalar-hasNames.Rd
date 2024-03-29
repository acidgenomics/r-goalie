% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-hasNames.R
\name{check-scalar-hasNames}
\alias{check-scalar-hasNames}
\alias{hasNames}
\title{Does the input have names?}
\usage{
hasNames(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Does the input have names?
}
\note{
Updated 2019-08-10.
}
\examples{
## TRUE ====
hasNames(datasets::mtcars)

## FALSE ====
hasNames(matrix())
hasNames(data.frame())
}
\seealso{
\code{assertive.properties::has_names()}.
}
