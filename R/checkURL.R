#' Does the Argument Contain a URL?
#'
#' @name checkURL
#' @aliases url
#' @inherit params
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Single URL.
#' checkURL(urls[[1L]])
#' checkURL(urls)  # fail
#'
#' ## Multiple URLs.
#' checkURLs(urls)
#' checkURLs(urls[[1L]])  # fail
NULL



# Single URL ===================================================================
#' @rdname checkURL
#' @export
checkURL <- function(x) {
    ok <- testString(x)
    if (!ok) {
        return("Must contain string")
    }

    ok <- vapply(
        X = x,
        FUN = function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
    if (!ok) {
        return("URL must begin with `http(s)` or `ftp` and contain `://`.")
    }

    TRUE
}



#' @rdname checkURL
#' @export
check_url <- checkURL



#' @rdname checkURL
#' @export
testURL <- makeTestFunction(checkURL)


#' @rdname checkURL
#' @export
test_url <- testURL



#' @rdname checkURL
#' @export
assertURL <- makeAssertionFunction(checkURL)



#' @rdname checkURL
#' @export
assert_url <- assertURL



#' @rdname checkURL
#' @export
expect_url <- makeExpectationFunction(checkURL)



# Multiple URLs ================================================================
#' @rdname checkURL
#' @export
checkURLs <- function(x) {
    if (testScalar(x)) {
        return("Use `checkURL()` for string")
    }
    all(vapply(
        X = x,
        FUN = testURL,
        FUN.VALUE = logical(1L)
    ))
}



#' @rdname checkURL
#' @export
check_urls <- checkURLs



#' @rdname checkURL
#' @export
testURLs <- makeTestFunction(checkURLs)



#' @rdname checkURL
#' @export
test_urls <- testURLs



#' @rdname checkURL
#' @export
assertAreURLs <- makeAssertionFunction(checkURLs)



#' @rdname checkURL
#' @export
assert_are_urls <- assertAreURLs



#' @rdname checkURL
#' @export
expect_are_urls <- makeExpectationFunction(checkURLs)
