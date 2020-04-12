#' Is the package installed?
#'
#' @name check-vector-isInstalled
#' @note Updated 2020-04-08.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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
    # Note that GitHub packages are "owner/repo", so use basename.
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
