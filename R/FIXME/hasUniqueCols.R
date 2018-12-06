# FIXME Simplify and don't use a generic.
# Use `requireNamespace("SummarizedExperiment", quietly = TRUE)` for SE section.



#' Does the Object Contain Columns with Unique Values?
#'
#' Checks a matrix for duplicated columns, which reprent samples. Duplicate
#' rows are allowed here, because many genes (rows) can contain all zeros.
#'
#' @section SummarizedExperiment:
#'
#' If the object is a `SummarizedExperiment`, then the primary `assay()` matrix
#' is checked for duplicated columns.
#'
#' @name hasUniqueCols
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit params
#'
#' @examples
#' ## Unique.
#' x <- matrix(data = seq_len(20L), ncol = 2L)
#' hasUniqueCols(x)
#'
#' ## Duplicated.
#' x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
#' hasUniqueCols(x)
NULL



hasUniqueCols.matrix <-  # nolint
    function(x) {
        # Check for >= 2 samples.
        if (!ncol(x) > 1L) {
            return(FALSE)
        }
        # We're using the S3 assay `duplicated()` method here, which supports
        # MARGIN, so we can check across the columns.
        !any(duplicated(x, MARGIN = 2L))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("matrix"),
    definition = hasUniqueCols.matrix
)



# Use primary assay matrix when dealing with SE.
hasUniqueCols.SummarizedExperiment <-  # nolint
    function(x) {
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        hasUniqueCols(SummarizedExperiment::assay(x))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("SummarizedExperiment"),
    definition = hasUniqueCols.SummarizedExperiment
)



# For other data types, attempt to coerce to standard matrix before check.
# This approach will work for sparseMatrix and data.frame.
hasUniqueCols.ANY <-  # nolint
    function(x) {
        hasUniqueCols(as(x, "matrix"))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("ANY"),
    definition = hasUniqueCols.ANY
)



#' @rdname hasUniqueCols
#' @export
assertHasUniqueCols <- function(x) {
    assert(hasUniqueCols(x))
    invisible(x)
}



#' @rdname hasUniqueCols
#' @export
assert_has_unique_cols <- assertHasUniqueCols
