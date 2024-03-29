% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate}
\alias{validate}
\title{Validate an S4 class}
\usage{
validate(..., msg = NULL)
}
\arguments{
\item{...}{Any number of R expressions that return \code{logical(1)}, each of
which should evaluate to \code{TRUE}. Rather than combining expressions with
\code{&&}, separate them by commas so that better error messages can be
generated.}

\item{msg}{\code{NULL} or \code{character(1)}.
Custom message to return.}
}
\value{
\code{TRUE} on success, or \code{character(1)} on failure, containing an error
message.
}
\description{
\code{validate()} is a variant of \code{assert()} that is specifically intended to be
used inside of an S4 validity method definition.
}
\details{
Like \code{assert()}, \code{validate()} returns \code{TRUE} on success. However, on failure
it returns a \code{character} instead of a \code{stop()} call. This is the current
recommended practice for defining S4 validity methods inside of a
\code{setValidity()} call. Refer to the documentation in the methods package,
specifically \code{validObject()} for detailed information on S4 validity methods.
}
\note{
Updated 2021-10-08.
}
\examples{
## TRUE ====
validate(
    is.atomic("example"),
    is.character("example")
)

## FALSE ====
validate(
    isFlag("xxx"),
    isPositive(-1)
)
}
\seealso{
\itemize{
\item \code{methods::setValidity()}.
\item \code{methods::validObject()}.
\item \code{assertthat::validate_that()}.
}
}
