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
#'
#' ## Fail ====
#' isURL("xxx")
#' isAURL(urls)
NULL



#' @describeIn isURL Supports multiple URLs.
#' @export
isURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    pattern <- "^(http(s)?|ftp)\\://.+"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    if (!all(ok)) {
        return(false(
            paste0(
                "%s does not contain a URL, ",
                "which must begin with ‘http(s)’ or ‘ftp’ and contain ‘://’."
            ),
            .xname
        ))
    }

    TRUE
}



#' @describeIn isURL Requires a single URL.
#' @export
isAURL <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isURL(x)
    if (!isTRUE(ok)) return(ok)

    TRUE
}



# Soft deprecated.
#' @rdname isURL
#' @usage NULL
#' @export
containsURL <- isURL



# Soft deprecated.
#' @rdname isURL
#' @usage NULL
#' @export
containsAURL <- isAURL
