#' Does the object contain quality control metrics?
#'
#' @name check-scalar-hasMetrics
#' @note Updated 2019-08-12.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#' @param colnames `character`.
#'   Column names in [`colData()`][SummarizedExperiment::colData] containing
#'   expected quality control metrics.
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data(RangedSummarizedExperiment, package = "acidtest")
#'     x <- RangedSummarizedExperiment
#'     hasMetrics(x)
#' }
NULL



hasMetrics <-
    function(
        x,
        colnames = c("nCount", "nFeature"),
        .xname = getNameInParent(x)
    ) {
        requireNamespace("SummarizedExperiment", quietly = TRUE)
        assert(
            is(x, "SummarizedExperiment"),
            isCharacter(colnames)
        )
        colData <- SummarizedExperiment::colData(x)
        ok <- isSubset(x = colnames, y = colnames(colData))
        if (!isTRUE(ok)) {
            setdiff <- setdiff(colnames, colnames(colData))
            return(false(
                "'%s' does not contain metrics in 'colData()': %s.",
                .xname,
                toString(setdiff, width = 100L)
            ))
        }
        TRUE
    }
