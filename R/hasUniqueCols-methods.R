#' Does the Object Contain Unique Columns?
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



hasUniqueCols.ANY <-  # nolint
    function(x) {
        assert_has_dims(x)
        # Check for >= 2 samples.
        if (!ncol(x) >= 2L) {
            return(FALSE)
        }
        # Check that none of the samples are duplicated.
        !any(duplicated(x, MARGIN = 2L))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("ANY"),
    definition = hasUniqueCols.ANY
)



hasUniqueCols.sparseMatrix <-  # nolint
    function(x) {
        hasUniqueCols(as.matrix(x))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("sparseMatrix"),
    definition = hasUniqueCols.sparseMatrix
)



# Use primary assay matrix when dealing with SE.
hasUniqueCols.SummarizedExperiment <-  # nolint
    function(x) {
        hasUniqueCols(assay(x))
    }



#' @rdname hasUniqueCols
#' @export
setMethod(
    f = "hasUniqueCols",
    signature = signature("SummarizedExperiment"),
    definition = hasUniqueCols.SummarizedExperiment
)



#' @rdname hasUniqueCols
#' @export
assertHasUniqueCols <- function(x) {
    assert_that(hasUniqueCols(x))
}
