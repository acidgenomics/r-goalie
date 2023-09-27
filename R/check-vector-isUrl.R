#' Does the input contain a URL?
#'
#' @name check-vector-isUrl
#' @note Updated 2023-09-15.
#'
#' @details
#' This assert check is intended to be simple and does not check to see if the
#' URL exists (is active). For that, refer to `isExistingUrl` instead.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' urls <- c("https://www.r-project.org/", "ftp://r-project.org/")
#'
#' ## TRUE ====
#' isUrl(urls)
#' isAUrl(urls[[1L]])
#' allAreUrls(urls)
#'
#' ## FALSE ====
#' isUrl("xxx")
#' isAUrl(urls)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isUrl Vectorized.
#' @export
isUrl <- function(x) {
    ok <- is(x, "url")
    if (isTRUE(ok)) {
        names(ok) <- "connection"
        return(ok)
    }
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isMatchingRegex(x = x, pattern = "^[^:/]+\\://.+$")
    if (!all(ok)) {
        return(setCause(ok, false = "not URL"))
    }
    enc <- URLencode(x)
    ok <- x == enc
    names(ok) <- x
    setCause(ok, false = "not encoded")
}



## Scalar ======================================================================

#' @describeIn check-vector-isUrl Scalar. Requires a single URL.
#' @export
isAUrl <- function(x) {
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isUrl(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isUrl Scalar. Checks that all strings are URLs.
#' @export
allAreUrls <- function(x) {
    ok <- isUrl(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
