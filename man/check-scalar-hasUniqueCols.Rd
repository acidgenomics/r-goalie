% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-scalar-hasUniqueCols.R
\name{check-scalar-hasUniqueCols}
\alias{check-scalar-hasUniqueCols}
\alias{hasUniqueCols}
\title{Does the input have columns with unique values?}
\usage{
hasUniqueCols(x)
}
\arguments{
\item{x}{Object.}
}
\value{
\code{TRUE} on success;
\code{FALSE} on failure, with cause set.
}
\description{
Checks a matrix for duplicated columns, which reprent samples. Duplicate
rows are allowed here, because many genes (rows) can contain all zeros.
}
\note{
Updated 2019-08-10.
}
\section{SummarizedExperiment}{


If the object is a \code{SummarizedExperiment}, then the primary
\link[SummarizedExperiment:SummarizedExperiment-class]{assay} matrix is checked for duplicated
columns.
}

\examples{
## TRUE ====
x <- matrix(data = seq_len(20L), ncol = 2L)
hasUniqueCols(x)

## FALSE ====
x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
hasUniqueCols(x)
}
