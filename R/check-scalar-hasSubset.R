#' Does the object contain a subset of data?
#'
#' @name check-scalar-hasSubset
#' @note Updated 2020-04-08.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#' @param x `Annotated`.
#' @param metadata `character`.
#'   Names in [`metadata()`][S4Vectors::metadata] that denote object is subset.
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data(RangedSummarizedExperiment, package = "acidtest")
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
        requireNamespaces("S4Vectors")
        assert(
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
