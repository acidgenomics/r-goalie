#' Does the input contain a BiocParallel parameter?
#'
#' @name check-scalar-isBiocParallelParam
#' @note Updated 2023-07-21.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `BiocParallel::bpparam`.
#'
#' @examples
#' ## TRUE ====
#' if (isInstalled("BiocParallel")) {
#'     isBiocParallelParam(BiocParallel::bpparam())
#' }
#'
#' ## FALSE ====
#' isBiocParallelParam(list())
NULL



#' @rdname check-scalar-isBiocParallelParam
#' @export
isBiocParallelParam <- function(x, .xname = getNameInParent(x)) {
    ok <- all(
        identical(
            attributes(class(x))[["package"]],
            "BiocParallel"
        ),
        grepl("Param$", class(x))
    )
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not a BiocParallel param.", .xname))
    }
    TRUE
}
