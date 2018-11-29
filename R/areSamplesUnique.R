#' Does the Object Contain Unique Samples?
#'
#' Checks a matrix for duplicated columns, which reprent samples. Duplicate
#' rows are allowed here, because many genes (rows) can contain all zeros.
#'
#' @section SummarizedExperiment:
#'
#' If the object is a `SummarizedExperiment`, then the primary `assay()` matrix
#' is checked for duplicated columns.
#'
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit params
#' @export
#'
#' @examples
#' ## Unique.
#' x <- matrix(data = seq_len(20L), ncol = 2L)
#' areSamplesUnique(x)
#'
#' ## Duplicated.
#' x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
#' areSamplesUnique(x)
areSamplesUnique <- function(x) {
    assert_has_dims(x)

    # Get primary assay matrix, when dealing with SE.
    if (is(x, "SummarizedExperiment")) {
        x <- assay(x)
    }

    # Check for >= 2 samples.
    if (!ncol(x) >= 2L) {
        return(FALSE)
    }

    # Check that none of the samples are duplicated.
    !any(duplicated(x, MARGIN = 2L))
}
