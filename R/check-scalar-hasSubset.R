#' Does the object contain a subset of data?
#'
#' @name check-scalar-hasSubset
#' @inherit check return
#' @note Updated 2023-09-29.
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
    function(x, metadata = "subset") {
        if (!is(x, "Annotated")) {
            return(false(
                "{.var %s} is not {.cls %s} class.",
                .toName(x), "Annotated"
            ))
        }
        requireNamespaces("S4Vectors")
        m1 <- metadata
        m2 <- names(S4Vectors::metadata(x))
        ok <- isSubset(x = m1, y = m2)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't contain slots in {.fun %s}: %s.",
                .toName(x), "metadata",
                toString(setdiff(m1, m2), width = 100L)
            ))
        }
        TRUE
    }
