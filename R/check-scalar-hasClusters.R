#' Does the input object contain clusters?
#'
#' @name check-scalar-hasClusters
#' @note Updated 2022-03-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `AcidGenerics::clusters`.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## TRUE ====
#' if (requireNamespace("AcidSingleCell", quietly = TRUE)) {
#'     hasClusters(SingleCellExperiment_Seurat)
#' }
#'
#' ## FALSE ====
#' hasClusters(list())
NULL



#' @rdname check-scalar-hasClusters
#' @export
hasClusters <- function(x, .xname = getNameInParent(x)) {
    assert(requireNamespace("AcidGenerics", quietly = TRUE))
    ok <- tryCatch(
        expr = is.factor(clusters(x)),
        error = function(e) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} does not contain clusters.", .xname))
    }
    return(TRUE)
}
