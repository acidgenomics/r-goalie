% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-vector-isExistingUrl.R
\name{check-vector-isExistingUrl}
\alias{check-vector-isExistingUrl}
\alias{isExistingUrl}
\alias{isAnExistingUrl}
\alias{allAreExistingUrls}
\title{Does the input contain an existing (active) URL?}
\usage{
isExistingUrl(x)

isAnExistingUrl(x)

allAreExistingUrls(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Does the input contain an existing (active) URL?
}
\details{
Supports HTTPS, HTTP, and FTP protocols.
}
\section{Functions}{
\itemize{
\item \code{isExistingUrl()}: Vectorized.

\item \code{isAnExistingUrl()}: Scalar. Requires a single URL.

\item \code{allAreExistingUrls()}: Scalar. Checks that all strings are
existing URLs.

}}
\note{
Updated 2023-10-02.
}
\examples{
## TRUE ====
isAnExistingUrl("https://acidgenomics.com/")

## FALSE ====
isAnExistingUrl("https://failwhale.acidgenomics.com/")
}
\seealso{
\itemize{
\item \code{curlGetHeaders()}: Amazing function with good timeout control.
\item \code{open.connection()}: Base method with no dependencies, but prone to hang
with poor timeout control.
\item \code{RCurl::url.exists()}: Seems to be good but adds external dependency.
\item urlchecker and curl packages.
\item https://github.com/r-lib/urlchecker/blob/main/inst/tools/urltools.R
\item https://stackoverflow.com/questions/52911812
\item https://stackoverflow.com/a/17620732/3911732
}
}
