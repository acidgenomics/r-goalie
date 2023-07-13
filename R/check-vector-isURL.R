#' Does the input contain a URL?
#'
#' @name check-vector-isURL
#' @note Updated 2021-07-19.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## TRUE ====
#' isURL(urls)
#' isAURL(urls[[1L]])
#' allAreURLs(urls)
#'
#' ## FALSE ====
#' isURL("xxx")
#' isAURL(urls)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isURL Vectorized.
#' @export
isURL <- function(x, .xname = getNameInParent(x)) {
    ok <- is(x, "url")
    if (isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    pattern <- "^(http(s)?|ftp)\\://.+"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    setCause(ok, false = "not URL")
}



## Scalar ======================================================================

#' @describeIn check-vector-isURL Scalar. Requires a single URL.
#' @export
isAURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalar(x = x, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isURL(x = x, .xname = .xname)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isURL Scalar. Checks that all strings are URLs.
#' @export
allAreURLs <- function(x, .xname = getNameInParent(x)) {
    ok <- isURL(x = x, .xname = .xname)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
