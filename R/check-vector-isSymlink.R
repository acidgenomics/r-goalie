#' Does the input contain a symbolic link?
#'
#' @name check-vector-isSymlink
#' @note Updated 2023-10-02.
#'
#' @details
#' Supported on Linux and macOS but not Windows.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `file.info()`.
#' - `Sys.readlink()`.
#'
#' @examples
#' if (!isWindows()) {
#'     from <- "from.txt"
#'     to <- "to.txt"
#'     file.create(from)
#'     file.symlink(from = from, to = to)
#'     isSymlink(c(from, to))
#'     unlink(c(from, to))
#' }
NULL



## Vector ======================================================================

#' @describeIn check-vector-isSymlink Vectorized.
#' @export
isSymlink <- function(x) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "not character"))
    }
    ok <- !isWindows()
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "windows"))
    }
    ok <- file.exists(x)
    if (!all(ok)) {
        return(setCause(ok, false = "doesn't exist"))
    }
    ok <- nzchar(Sys.readlink(x), keepNA = TRUE)
    setCause(ok, false = "not symlink")
}



## Scalar ======================================================================

#' @describeIn check-vector-isSymlink Scalar.
#' @export
isASymlink <- function(x, nullOk = FALSE) {
    if (isTRUE(nullOk) && is.null(x)) {
        return(TRUE)
    }
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isSymlink(x)
    if (!all(ok)) {
        return(ok)
    }
    TRUE
}



#' @describeIn check-vector-isSymlink Scalar.
#' @export
allAreSymlinks <- function(x) {
    ok <- isSymlink(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
