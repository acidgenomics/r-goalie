% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-vector-isSystemCommand.R
\name{check-vector-isSystemCommand}
\alias{check-vector-isSystemCommand}
\alias{isSystemCommand}
\title{Is a system command installed?}
\usage{
isSystemCommand(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Is a system command installed?
}
\section{Functions}{
\itemize{
\item \code{isSystemCommand}: Vectorized.
}}

\note{
Updated 2020-04-08.
}
\examples{
## TRUE ====
isSystemCommand(c("cp", "rm"))

## FALSE ====
isSystemCommand(c("AAA", "BBB"))
}
