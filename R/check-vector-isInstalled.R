#' Is the package installed?
#'
#' @name check-vector-isInstalled
#' @note Updated 2020-04-08.
#'
#' @inherit check
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
isInstalled <- function(x) {
    stopifnot(requireNamespace("utils", quietly = TRUE))
    # Note that GitHub packages are "user/repo", so use basename.
    ok <- basename(x) %in% rownames(installed.packages())
    names(ok) <- toNames(x)
    setCause(ok, false = "not installed")
}



## Scalar ======================================================================
#' @describeIn check-vector-isInstalled Scalar.
#' @export
allAreInstalled <- function(x) {
    ok <- isInstalled(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
