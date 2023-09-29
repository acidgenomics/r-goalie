#' Does the input contain a file?
#'
#' @name check-vector-isFile
#' @note Updated 2023-09-29.
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
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    cn <- toCauseNames(x)
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        names(ko) <- cn
        return(setCause(ko, false = "not character"))
    }
    ok <- !dir.exists(x)
    if (!all(ok)) {
        names(ok) <- cn
        return(setCause(ok, false = "dir"))
    }
    ok <- file.exists(x)
    names(ok) <- cn
    setCause(ok, false = "not file")
}



## Scalar ======================================================================

#' @describeIn check-vector-isFile Scalar.
#' @export
isAFile <- function(x, nullOk = FALSE) {
    if (isTRUE(nullOk) && is.null(x)) {
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



#' @describeIn check-vector-isFile Scalar.
#' @export
allAreFiles <- function(x) {
    ok <- isFile(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
