% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system-check-scalar-hasInternet.R
\name{check-scalar-hasInternet}
\alias{check-scalar-hasInternet}
\alias{hasInternet}
\title{Does the current session have an Internet connection?}
\usage{
hasInternet()
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Does the current session have an Internet connection?
}
\note{
Updated 2023-08-24.
}
\examples{
hasInternet()
}
\seealso{
\itemize{
\item \code{isAnExistingUrl()}.
\item \code{Biobase::testBioCConnection()}.
}
}
