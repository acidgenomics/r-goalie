#' Does the input object contain multiple samples?
#'
#' @name check-scalar-hasMultipleSamples
#' @note Updated 2022-03-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `sampleNames`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## TRUE ====
#' if (requireNamespace("AcidSingleCell", quietly = TRUE)) {
#'     (SingleCellExperiment_splatter)
#' }
#'
#' ## FALSE ====
#' hasMultipleSamples(list())
NULL



#' @rdname check-scalar-hasMultipleSamples
#' @export
hasMultipleSamples <- function(x, .xname = getNameInParent(x)) {
    ok <- is(x, "SummarizedExperiment")
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not {.cls SummarizedExperiment}.", .xname))
    }
    requireNamespaces("Biobase")
    ok <- tryCatch(
        expr = {
            length(Biobase::sampleNames(x)) > 1L
        },
        error = function(e) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} does not contain multiple samples.", .xname))
    }
    TRUE
}
