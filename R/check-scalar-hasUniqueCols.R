#' Does the input have columns with unique values?
#'
#' Checks a matrix for duplicated columns, which reprent samples. Duplicate
#' rows are allowed here, because many genes (rows) can contain all zeros.
#'
#' @section SummarizedExperiment:
#'
#' If the object is a `SummarizedExperiment`, then the primary
#' [assay][SummarizedExperiment::assay] matrix is checked for duplicated
#' columns.
#'
#' @name check-scalar-hasUniqueCols
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' x <- matrix(data = seq_len(20L), ncol = 2L)
#' hasUniqueCols(x)
#'
#' ## FALSE ====
#' x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
#' hasUniqueCols(x)
NULL



#' @rdname check-scalar-hasUniqueCols
#' @export
hasUniqueCols <- function(x) {
    ## Coerce SummarizedExperiment to (assay) matrix, if necessary.
    if (is(x, "SummarizedExperiment")) {
        x <- .coerceSummarizedExperimentToMatrix(x)
    }
    ## Check for >= 2 samples.
    ok <- ncol(x) >= 2L
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} doesn't have >= 2 columns.",
            .toName(x)
        ))
    }
    ## Ensure coercion to matrix, so we can use the S3 assay method for
    ## `duplicated()` below.
    x <- as(x, "matrix")
    ## We're using the S3 assay `duplicated` method here, which supports
    ## MARGIN, so we can check across the columns.
    dupes <- duplicated(x, MARGIN = 2L)
    ok <- !any(dupes)
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} has duplicated columns at: %s",
            .toName(x), toString(which(dupes), width = 200L)
        ))
    }
    TRUE
}
