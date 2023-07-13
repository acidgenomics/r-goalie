#' Does the input contain a temporary file?
#'
#' @name check-vector-isTempFile
#' @note Updated 2023-07-13.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `tempfile()`.
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
#' @describeIn check-vector-isTempFile Vectorized.
#' @export
isTempFile <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- !bapply(X = x, FUN = dir.exists)
    if (!all(ok)) {
        return(setCause(ok, false = "dir"))
    }
    ok <- bapply(X = x, FUN = file.exists)
    setCause(ok, false = "not file")
}



## Scalar ======================================================================
#' @describeIn check-vector-isTempFile Scalar.
#' @export
isATempFile <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isFile(x)
    if (!all(ok)) {
        return(ok)
    }
    TRUE
}



#' @describeIn check-vector-isTempFile Scalar.
#' @export
allAreTempFiles <- function(x) {
    ok <- isTempFile(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
