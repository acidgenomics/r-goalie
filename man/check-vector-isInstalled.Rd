% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system-check-vector-isInstalled.R
\name{check-vector-isInstalled}
\alias{check-vector-isInstalled}
\alias{isInstalled}
\alias{allAreInstalled}
\title{Is the package installed?}
\usage{
isInstalled(x, lib = NULL)

allAreInstalled(x, lib = NULL)
}
\arguments{
\item{x}{Object.}

\item{lib}{\code{character(1)} or \code{NULL}.
R library location.
If left \code{NULL}, includes all known library paths defined in \code{.libPaths}.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Is the package installed?
}
\section{Functions}{
\itemize{
\item \code{isInstalled()}: Vectorized.

\item \code{allAreInstalled()}: Scalar.

}}
\note{
Updated 2023-09-29.
}
\examples{
## TRUE ====
isInstalled(c("base", "utils"))

## FALSE ====
isInstalled(c("AAA", "BBB"))
}
