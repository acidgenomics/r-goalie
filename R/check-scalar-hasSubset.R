#' Does the object contain a subset of data?
#'
#' @name check-scalar-hasSubset
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param x `Annotated`.
#'
#' @param metadata `character`.
#' Names in [`metadata()`][S4Vectors::metadata] that denote object is subset.
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
    function(x,
             metadata = "subset",
             .xname = getNameInParent(x)) {
        if (!is(x, "Annotated")) {
            return(false(
                "{.var %s} is not {.cls %s} class.",
                .xname, "Annotated"
            ))
        }
        requireNamespaces("S4Vectors")
        m1 <- metadata
        m2 <- names(S4Vectors::metadata(x))
        ok <- isSubset(x = m1, y = m2)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't contain slots in {.fun %s}: %s.",
                .xname, "metadata",
                toString(setdiff(m1, m2), width = 100L)
            ))
        }
        TRUE
    }
