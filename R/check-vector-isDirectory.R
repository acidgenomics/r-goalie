#' Does the input contain a directory?
#'
#' @name check-vector-isDirectory
#' @note Updated 2020-06-26.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `dir.exists()`.
#' - `R.utils::isDirectory()`.
#'
#' @examples
#' ## TRUE ====
#' isDirectory(c("~", "~"))
#' isADirectory("~")
#'
#' ## FALSE ====
#' isDirectory(1L)
NULL



## Vector ======================================================================
#' @describeIn check-vector-isDirectory Vectorized.
#' @export
isDirectory <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)
    ok <- bapply(X = x, FUN = dir.exists)
    setCause(ok, false = "not dir")
}



## Scalar ======================================================================
#' @describeIn check-vector-isDirectory Scalar.
#' @export
isADirectory <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) return(TRUE)
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)
    ok <- isDirectory(x)
    if (!isTRUE(ok)) return(ok)
    TRUE
}

#' @describeIn check-vector-isDirectory Scalar.
#' @export
allAreDirectories <- function(x) {
    ok <- isDirectory(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



## Aliases =====================================================================
#' @describeIn check-vector-isDirectory Short alias for [isDirectory()].
#' @export
isDir <- isDirectory

#' @describeIn check-vector-isDirectory Short alias for [isADirectory()].
#' @export
isADir <- isADirectory

#' @describeIn check-vector-isDirectory Short alias for [allAreDirectories()].
#' @export
allAreDirs <- allAreDirectories
