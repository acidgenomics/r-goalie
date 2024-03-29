% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-isNumber.R,
%   R/check-scalar-isScalar.R
\name{isNumber}
\alias{isNumber}
\alias{check-scalar-isScalar}
\alias{isScalar}
\alias{isScalarAtomic}
\alias{isScalarCharacter}
\alias{isScalarDouble}
\alias{isScalarInteger}
\alias{isScalarIntegerish}
\alias{isScalarList}
\alias{isScalarLogical}
\alias{isScalarNumeric}
\alias{isScalarVector}
\alias{isNonScalar}
\title{Is the input scalar?}
\usage{
isNumber(x, nullOk = FALSE)

isScalar(x, nullOk = FALSE)

isScalarAtomic(x, nullOk = FALSE)

isScalarCharacter(x, nullOk = FALSE)

isScalarDouble(x, nullOk = FALSE)

isScalarInteger(x, nullOk = FALSE)

isScalarIntegerish(x, nullOk = FALSE)

isScalarList(x, nullOk = FALSE)

isScalarLogical(x, nullOk = FALSE)

isScalarNumeric(x, nullOk = FALSE)

isScalarVector(x, nullOk = FALSE)

isNonScalar(x)
}
\arguments{
\item{x}{Object.}

\item{nullOk}{\code{logical(1)}.
If set to \code{TRUE}, \code{x} may also be \code{NULL}.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Scalar represents a length of 1.
}
\section{Functions}{
\itemize{
\item \code{isNumber()}: Alias for \code{\link[=isScalarNumeric]{isScalarNumeric()}}.

}}
\note{
Updated 2021-10-08.
}
\examples{
## TRUE ====
isScalar("a")
isScalarInteger(1L)
isScalarIntegerish(1)

## FALSE ====
isScalar(NULL)
isScalar(c("a", "b"))
}
\seealso{
\itemize{
\item \code{help(topic = "scalar-type-predicates", package = "rlang")}.
\item \code{assertive.properties::is_scalar()}.
\item \code{rlang::is_scalar_atomic()}.
\item \code{rlang::is_scalar_character()}.
\item \code{rlang::is_scalar_double()}.
\item \code{rlang::is_scalar_integer()}.
\item \code{rlang::is_scalar_integerish()}.
\item \code{rlang::is_scalar_list()}.
\item \code{rlang::is_scalar_logical()}
\item \code{rlang::is_scalar_vector()}.
}
}
