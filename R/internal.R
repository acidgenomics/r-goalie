#' Assert check
#'
#' @name check
#' @keywords internal
#'
#' @return
#' `TRUE` on success;
#' `FALSE` on failure, with cause set.
NULL



## Using primary assay here.
.coerceSummarizedExperimentToMatrix <- function(object) {
    assert(requireNamespace("SummarizedExperiment", quietly = TRUE))
    SummarizedExperiment::assay(object)
}



## @seealso `base::stopifnot()`.
## Updated 2019-10-21.
.deparse <-
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            paste(ch[[1L]], "....")  # nocov
        } else {
            ch
        }
    }



## @seealso `assertive.properties::DIM()`.
## Updated 2019-07-15.
.dim <- function(x) {
    dim <- dim(x)
    if (is.null(dim)) {
        length(x)
    } else {
        dim
    }
}



## Updated 2019-08-10.
.hasCause <- function(x) {
    cause <- cause(x)
    if (
        length(cause) != 1L &&
        !identical(length(x), length(cause))
    ) {
        FALSE
    } else {
        TRUE
    }
}



.tolerance <- 100L * .Machine[["double.eps"]]
