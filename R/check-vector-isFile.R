#' Does the input contain a file?
#'
#' @name check-vector-isFile
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso `R.utils::isFile()`.
#'
#' ```
#' getS3method(
#'     f = "isFile",
#'     class = "default",
#'     envir = asNamespace("R.utils")
#' )
#' ```
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
## Updated 2019-07-15.
isFile <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    ok <- bapply(X = x, FUN = file.exists)
    setCause(ok, false = "not file")
}



## Scalar ======================================================================
#' @describeIn check-vector-isFile Scalar.
#' @export
## Updated 2019-07-15.
isAFile <- function(x, nullOK = FALSE) {
    ## Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isFile(x)
    if (!all(ok)) return(ok)

    TRUE
}



#' @describeIn check-vector-isFile Scalar.
#' @export
## Updated 2019-07-15.
allAreFiles <- function(x) {
    ok <- isFile(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
