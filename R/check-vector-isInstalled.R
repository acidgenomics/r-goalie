#' Is the package installed?
#'
#' @name check-vector-isInstalled
#' @note Updated 2021-08-22.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param lib `character(1)` or `NULL`.
#'   R library location.
#'   If left `NULL`, includes all known library paths defined in `.libPaths`.
#'
#' @examples
#' ## TRUE ====
#' isInstalled(c("base", "utils"))
#'
#' ## FALSE ====
#' isInstalled(c("AAA", "BBB"))
NULL



## Vector ======================================================================
#' @describeIn check-vector-isInstalled Vectorized.
#' @export
isInstalled <- function(x, lib = NULL) {
    df <- installed.packages(lib.loc = lib)
    # Note that GitHub packages are "owner/repo", so use basename.
    ok <- basename(x) %in% rownames(df)
    names(ok) <- .toNames(x)
    setCause(ok, false = "not installed")
}



## Scalar ======================================================================
#' @describeIn check-vector-isInstalled Scalar.
#' @export
allAreInstalled <- function(x, lib = NULL) {
    ok <- isInstalled(x, lib = lib)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
