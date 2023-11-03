#' Does the input contain an AWS S3 URI?
#'
#' @name check-vector-isAwsS3Uri
#' @note Updated 2023-11-03.
#'
#' @details
#' This assert check is intended to be simple and does not check to see if the
#' URL exists (is active). For that, refer to `isExistingUrl` instead.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' uris <- c("s3://acidgenomics.com/", "s3://koopa.acidgenomics.com/")
#'
#' ## TRUE ====
#' isAwsS3Uri(uris)
#' isAnAwsS3Uri(uris[[1L]])
#' allAreAwsS3Uris(urls)
#'
#' ## FALSE ====
#' isAwsS3Uri("xxx")
#' isAnAwsS3Uri(uri)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isAwsS3Uri Vectorized.
#' @export
isAwsS3Uri <- function(x) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "not character"))
    }
    ok <- isMatchingRegex(x = x, pattern = "^s3\\://.+$")
    if (!all(ok)) {
        return(setCause(ok, false = "not AWS S3 URI"))
    }
    requireNamespaces("utils")
    enc <- utils::URLencode(x)
    ok <- x == enc
    setCause(ok, false = "not encoded")
}



## Scalar ======================================================================

#' @describeIn check-vector-isAwsS3Uri Scalar. Requires a single URI.
#' @export
isAnAwsS3Uri <- function(x) {
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isAwsS3Uri(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isAwsS3Uri Scalar. Checks that all strings are URIs.
#' @export
allAreAwsS3Uris <- function(x) {
    ok <- isAwsS3Uri(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
