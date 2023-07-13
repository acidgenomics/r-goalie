#' Does the input contain a temporary file (that exists on disk)?
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
#' ## The temporary file must exist on disk.
#'
#' ## TRUE ====
#' x <- tempfile()
#' file.create(x)
#' file.exists(x)
#' isATempFile(x)
#' unlink(x)
#'
#' ## FALSE ====
#' x <- tempfile()
#' file.exists(x)
#' isATempFile(x)
NULL



## Vector ======================================================================
#' @describeIn check-vector-isTempFile Vectorized.
#' @export
isTempFile <- function(x) {
    ok <- allAreFiles(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- allAreMatchingFixed(x = x, pattern = tempdir())
    if (!isTRUE(ok)) {
        return(ok)
    }
    rep(x = TRUE, times = length(x))
}



## Scalar ======================================================================
#' @describeIn check-vector-isTempFile Scalar.
#' @export
isATempFile <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isTempFile(x)
    if (!isTRUE(ok)) {
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
