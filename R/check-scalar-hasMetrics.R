#' Does the object contain quality control metrics?
#'
#' @name check-scalar-hasMetrics
#' @inherit check return
#' @note Updated 2021-01-04.
#'
#' @param x `SummarizedExperiment`.
#'
#' @param colData `character`.
#' Column names in [`colData()`][SummarizedExperiment::colData] containing
#' expected quality control metrics.
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data(RangedSummarizedExperiment, package = "AcidTest")
#'     x <- RangedSummarizedExperiment
#'     hasMetrics(x)
#' }
NULL



#' @rdname check-scalar-hasMetrics
#' @export
hasMetrics <-
    function(x, colData = c("nCount", "nFeature")) {
        if (!is(x, "SummarizedExperiment")) {
            return(FALSE)
        }
        requireNamespaces("SummarizedExperiment")
        c1 <- colData
        c2 <- colnames(SummarizedExperiment::colData(x))
        ok <- isSubset(c1, c2)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't contain metrics in {.fun %s}: %s.",
                toCauseName(x), "colData",
                toString(setdiff(c1, c2), width = 100L)
            ))
        }
        TRUE
    }
