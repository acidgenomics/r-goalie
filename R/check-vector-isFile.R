#' Does the input contain a file?
#'
#' @name isFile
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



#' @describeIn isFile Vectorized.
#' @export
isFile <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    # Note that `file.exists()` below will return TRUE on directory.
    ok <- !isDirectory(x)
    if (!all(ok)) return(ok)

    bapply(X = x, FUN = file.exists)
}



#' @describeIn isFile Scalar variant.
#' @export
isAFile <- function(x, nullOK = FALSE) {
    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isFile(x)
    if (!all(ok)) return(ok)

    TRUE
}



#' @describeIn isFile Scalar variant.
#' @export
allAreFiles <- function(x) {
    ok <- isFile(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
