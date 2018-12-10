#' Does the Input Contain a URL?
#'
#' @name containsURL
#' @inherit params
#'
#' @param string `logical(1)`. Require match against string?
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



.containsURL <- function(x) {
    if (!is.character(x)) {
        return("Must contain character")
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
        return("URL must begin with `http(s)` or `ftp` and contain `://`.")
    }

    TRUE
}



#' @describeIn containsURL Supports multiple URLs.
#' @export
containsURL <- makeTestFunction(.containsURL)



#' @describeIn containsURL Requires a single URL.
#' @export
containsAURL <- function(x) {
    isScalar(x) && containsURL(x)
}
