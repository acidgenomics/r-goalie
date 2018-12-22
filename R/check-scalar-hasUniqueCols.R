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
#' @export
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' x <- matrix(data = seq_len(20L), ncol = 2L)
#' hasUniqueCols(x)
#'
#' ## Fail ====
#' x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
#' hasUniqueCols(x)
hasUniqueCols <- function(x, .xname = getNameInParent(x)) {
    # Coerce SummarizedExperiment to (assay) matrix, if necessary.
    if (is(x, "SummarizedExperiment")) {
        x <- .coerceSummarizedExperimentToMatrix(x)
    }

    # Check for >= 2 samples.
    ok <- ncol(x) > 1L
    if (!isTRUE(ok)) {
        return(false("%s does not have > 1 columns.", .xname))
    }

    # Ensure coercion to matrix, so we can use the S3 assay method for
    # `duplicated()` below.
    x <- as(x, "matrix")

    # We're using the S3 assay `duplicated` method here, which supports
    # MARGIN, so we can check across the columns.
    dupes <- duplicated(x, MARGIN = 2L)
    ok <- !any(dupes)
    if (!isTRUE(ok)) {
        return(false(
            "%s has duplicated columns at: %s",
            .xname, toString(which(dupes))
        ))
    }

    TRUE
}