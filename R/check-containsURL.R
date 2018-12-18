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
containsURL <- function(x) {
    xname <- getNameInParent(x)

    if (!is.character(x)) {
        return(false("% does not contain character.", xname))
    }

    ok <- all(vapply(
        X = x,
        FUN = function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    ))
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "%s does not contain a URL.\n",
                "URLs must begin with ‘http(s)’ or ‘ftp’ and contain ‘://’."
            ),
            xname
        ))
    }

    TRUE
}



#' @describeIn containsURL Requires a single URL.
#' @export
containsAURL <- function(x) {
    isScalar(x) && containsURL(x)
}



# Soft deprecate?
#' @rdname containsURL
#' @export
isURL <- containsURL



# Soft deprecate?
#' @rdname containsURL
#' @export
isAURL <- containsAURL
