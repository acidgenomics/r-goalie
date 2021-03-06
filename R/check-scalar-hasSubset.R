#' Does the object contain a subset of data?
#'
#' @name check-scalar-hasSubset
#' @note Updated 2021-01-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#' @param x `Annotated`.
#' @param metadata `character`.
#'   Names in [`metadata()`][S4Vectors::metadata] that denote object is subset.
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data(RangedSummarizedExperiment, package = "AcidTest")
#'     x <- RangedSummarizedExperiment
#'     hasSubset(x)
#' }
NULL



#' @rdname check-scalar-hasSubset
#' @export
hasSubset <-
    function(
        x,
        metadata = "subset",
        .xname = getNameInParent(x)
    ) {
        assert(
            requireNamespace("S4Vectors", quietly = TRUE),
            is(x, "Annotated"),
            isCharacter(metadata)
        )
        m1 <- metadata
        m2 <- names(S4Vectors::metadata(x))
        ok <- isSubset(m1, m2)
        if (!isTRUE(ok)) {
            return(false(
                "'%s' does not contain slots in 'metadata()': %s.",
                .xname,
                toString(setdiff(m1, m2), width = 100L)
            ))
        }
        TRUE
    }
