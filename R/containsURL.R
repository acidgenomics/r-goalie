#' Does the Argument Contain a URL?
#'
#' @name containsURL
#' @inherit params
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Pass ====
#' containsURL(urls[[1L]], string = TRUE)
#' containsURL(urls, string = FALSE)
#'
#' ## Fail ====
#' containsURL("xxx")
NULL



.containsURL <- function(x, string = FALSE) {
    assert(isFlag(string))

    if (isTRUE(string)) {
        if (!isString(x)) {
            return("Must contain string")
        }
    }

    if (!is(x, "character")) {
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



#' @rdname containsURL
#' @export
containsURL <- makeTestFunction(.containsURL)
