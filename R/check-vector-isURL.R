#' Does the input contain a URL?
#'
#' @name isURL
#' @inherit params
#'
#' @return `logical`.
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Pass ====
#' isURL(urls)
#' isAURL(urls[[1L]])
#' allAreURLs(urls)
#'
#' ## Fail ====
#' isURL("xxx")
#' isAURL(urls)
NULL



# vector =======================================================================
#' @describeIn isURL Vectorized.
#' @export
isURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    pattern <- "^(http(s)?|ftp)\\://.+"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    setCause(ok, false = "not URL")
}



# scalar =======================================================================
#' @describeIn isURL Scalar. Requires a single URL.
#' @export
isAURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isString(x = x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- isURL(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))

    TRUE
}



#' @describeIn isURL Scalar. Checks that all strings are URLs.
#' @export
allAreURLs <- function(x, .xname = getNameInParent(x)) {
    ok <- isURL(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
