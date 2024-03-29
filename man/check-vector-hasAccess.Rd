% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-vector-hasAccess.R
\name{check-vector-hasAccess}
\alias{check-vector-hasAccess}
\alias{hasAccess}
\alias{allHaveAccess}
\title{Check file system access rights}
\usage{
hasAccess(x, access = "r")

allHaveAccess(x, access = "r")
}
\arguments{
\item{x}{\code{character(1)}.
File or directory path(s).}

\item{access}{\code{character(1)}.
String containing any of these characters, including in combination:
\itemize{
\item \code{r}: read.
\item \code{w}: write.
\item \code{x}: execute.
}

Write and executable status cannot be checked on Windows.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Works for either file or directory paths.
}
\section{Functions}{
\itemize{
\item \code{hasAccess()}: Vectorized.

\item \code{allHaveAccess()}: Scalar.

}}
\note{
Updated 2023-09-29.
}
\examples{
## TRUE ====
hasAccess(c("~", "."))

## FALSE ====
hasAccess("xxx")
}
\seealso{
\itemize{
\item \code{file.access()}.
\item \code{checkmate::checkAccess()}.
}
}
