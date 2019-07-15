#' Does the input contain a directory?
#'
#' @name isDirectory
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso
#' `R.utils::isDirectory()`.
#'
#' ```
#' getS3method(
#'     f = "isDirectory",
#'     class = "default",
#'     envir = asNamespace("R.utils")
#' )
#' ```
#'
#' @examples
#' ## TRUE ====
#' isDirectory(c("~", "~"))
#' isADirectory("~")
#'
#' ## FALSE ====
#' isDirectory(1L)
NULL



#' @describeIn isDirectory Vectorized.
#' @export
# Updated 2019-07-15.
isDirectory <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    ok <- bapply(X = x, FUN = dir.exists)
    setCause(ok, false = "not dir")
}



#' @describeIn isDirectory Short alias for [isDirectory()].
#' @export
isDir <- isDirectory



#' @describeIn isDirectory Scalar.
#' @export
# Updated 2019-07-15.
isADirectory <- function(x, nullOK = FALSE) {
    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isDirectory(x)
    if (!isTRUE(ok)) return(ok)

    TRUE
}



#' @describeIn isDirectory Short alias for [isADirectory()].
#' @export
isADir <- isADirectory



#' @describeIn isDirectory Scalar variant.
#' @export
# Updated 2019-07-15.
allAreDirectories <- function(x) {
    ok <- isDirectory(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isDirectory Short alias for [allAreDirectories()].
#' @export
allAreDirs <- allAreDirectories
