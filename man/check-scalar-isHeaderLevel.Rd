% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-isHeaderLevel.R
\name{check-scalar-isHeaderLevel}
\alias{check-scalar-isHeaderLevel}
\alias{isHeaderLevel}
\title{Does the input contain a Markdown header level?}
\usage{
isHeaderLevel(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Markdown supports header levels \code{1}-\code{7} (\verb{<H1>}-\verb{<H7>}).
}
\note{
Updated 2023-09-29.
}
\examples{
## TRUE ====
isHeaderLevel(1)

## FALSE ====
isHeaderLevel(0)
}
