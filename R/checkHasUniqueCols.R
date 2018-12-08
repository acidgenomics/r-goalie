#' Does the Object Have Columns with Unique Values?
#'
#' Checks a matrix for duplicated columns, which reprent samples. Duplicate
#' rows are allowed here, because many genes (rows) can contain all zeros.
#'
#' @section SummarizedExperiment:
#'
#' If the object is a `SummarizedExperiment`, then the primary `assay()` matrix
#' is checked for duplicated columns.
#'
#' @name checkHasUniqueCols
#' @aliases hasUniqueCols
#' @author Michael Steinbaugh
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' x <- matrix(data = seq_len(20L), ncol = 2L)
#' checkHasUniqueCols(x)
#'
#' ## Fail ====
#' x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
#' checkHasUniqueCols(x)
checkHasUniqueCols <- function(x) {
    # Coerce SummarizedExperiment to matrix, if necessary.
    if (is(x, "SummarizedExperiment")) {
        x <- .coerceSummarizedExperimentToMatrix(x)
    }

    # Check for >= 2 samples.
    if (!ncol(x) > 1L) {
        return(FALSE)
    }

    # Ensure coercion to matrix, which can use the S3 assay method.
    x <- as(x, "matrix")

    # We're using the S3 assay `duplicated()` method here, which supports
    # MARGIN, so we can check across the columns.
    ok <- !any(duplicated(x, MARGIN = 2L))
    if (!ok) {
        # TODO Improve the message about which columns.
        return("Object has duplicate columns")
    }

    TRUE
}



#' @rdname checkHasUniqueCols
#' @export
check_has_unique_cols <- checkHasUniqueCols



#' @rdname checkHasUniqueCols
#' @export
testHasUniqueCols <- makeTestFunction(checkHasUniqueCols)



#' @rdname checkHasUniqueCols
#' @export
test_has_unique_cols <- testHasUniqueCols



#' @rdname checkHasUniqueCols
#' @export
assertHasUniqueCols <- makeAssertionFunction(checkHasUniqueCols)



#' @rdname checkHasUniqueCols
#' @export
assert_has_unique_cols <- assertHasUniqueCols



#' @rdname checkHasUniqueCols
#' @export
expect_has_unique_cols <- makeExpectationFunction(checkHasUniqueCols)
