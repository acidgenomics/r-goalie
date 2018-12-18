#' Does the input contain a URL?
#'
#' @name containsURL
#' @inherit params
#'
#' @return `logical`.
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Pass ====
#' containsURL(urls)
#' containsAURL(urls[[1L]])
#'
#' ## Fail ====
#' containsURL("xxx")
#' containsAURL(urls)
NULL



#' @describeIn containsURL Supports multiple URLs.
#' @export
containsURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }

    # TODO Switch to using `isMatchingRegex()` here.
    ok <- all(bapply(
        x,
        function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        }
    ))
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "%s does not contain a URL.\n",
                "URLs must begin with ‘http(s)’ or ‘ftp’ and contain ‘://’."
            ),
            .xname
        ))
    }

    TRUE
}



#' @describeIn containsURL Requires a single URL.
#' @export
containsAURL <- function(x) {
    isString(x) && containsURL(x)
}



# Soft deprecate?
#' @rdname containsURL
#' @export
isURL <- containsURL



# Soft deprecate?
#' @rdname containsURL
#' @export
isAURL <- containsAURL
