#' Does the input contain a compressed file?
#'
#' @details
#' Currently only performs a simple check, based on file extension match.
#'
#' @name check-vector-isCompressedFile
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - [isFile()].
#' - `compressFileExt`.
#'
#' @examples
#' ## TRUE ====
#' x <- c("sample1.fastq.gz", "sample2.fastq.bz2")
#' file.create(x)
#' isCompressedFile(x)
#' unlink(x)
#'
#' ## FALSE ====
#' x <- c("sample1.fastq", "sample2.fastq")
#' file.create(x)
#' isCompressedFile(x)
#' unlink(x)
NULL



## Vector ======================================================================
#' @describeIn check-vector-isCompressedFile Vectorized.
#' @export
isCompressedFile <- function(x) {
    ok <- allAreFiles(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- bapply(X = x, FUN = isMatchingRegex, pattern = compressExtPattern)
    setCause(ok, false = "no compress ext")
}



## Scalar ======================================================================
#' @describeIn check-vector-isCompressedFile Scalar.
#' @export
isACompressedFile <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCompressedFile(x)
    if (!all(ok)) {
        return(ok)
    }
    TRUE
}



#' @describeIn check-vector-isCompressedFile Scalar.
#' @export
allAreCompressedFiles <- function(x) {
    ok <- isCompressedFile(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
