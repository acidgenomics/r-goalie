#' Does the input contain a file?
#'
#' @name check-vector-isFile
#' @note Updated 2020-06-26.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `file.exists()`.
#' - `R.utils::isFile()`.
#'
#' @examples
#' ## TRUE ====
#' x <- "example.txt"
#' file.create(x)
#' isAFile(x)
#' unlink(x)
#'
#' ## FALSE ====
#' isFile(c("~", "."))
NULL



## Vector ======================================================================
#' @describeIn check-vector-isFile Vectorized.
#' @export
isFile <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)
    ok <- !bapply(X = x, FUN = dir.exists)
    if (!all(ok)) return(setCause(ok, false = "dir"))
    ok <- bapply(X = x, FUN = file.exists)
    setCause(ok, false = "not file")
}



## Scalar ======================================================================
#' @describeIn check-vector-isFile Scalar.
#' @export
isAFile <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) return(TRUE)
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)
    ok <- isFile(x)
    if (!all(ok)) return(ok)
    TRUE
}



#' @describeIn check-vector-isFile Scalar.
#' @export
allAreFiles <- function(x) {
    ok <- isFile(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
