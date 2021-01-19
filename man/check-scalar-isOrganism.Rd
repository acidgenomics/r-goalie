% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-isOrganism.R
\name{check-scalar-isOrganism}
\alias{check-scalar-isOrganism}
\alias{isOrganism}
\title{Is the string input a full Latin organism name?}
\usage{
isOrganism(x, nullOK = FALSE, .xname = getNameInParent(x))
}
\arguments{
\item{x}{Object.}

\item{nullOK}{\code{logical(1)}.
If set to \code{TRUE}, \code{x} may also be \code{NULL}.}

\item{.xname}{Name of object defined in \code{x}.
\emph{Not intended to be used directly.}}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Is the string input a full Latin organism name?
}
\details{
The binomial system of naming species uses Latin words. Each name has two
parts, the genus and the species. For example, human beings belong to the
genus \emph{Homo}, and our species is \emph{sapiens} -- so the scientific name is
\emph{Homo sapiens}.
}
\note{
Updated 2020-01-06.
}
\examples{
## TRUE ====
isOrganism("Homo sapiens")

## FALSE ====
isOrganism("Human")
isOrganism("homo_sapiens")
}