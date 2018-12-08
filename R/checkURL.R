#' Does the Argument Contain a URL?
#'
#' @name checkURL
#' @aliases url urls
#' @inherit params
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Pass ====
#' checkURL(urls[[1L]])
#' checkURLs(urls)
#'
#' ## Fail ====
#' checkURL("xxx")
#' checkURL(urls)
#' checkURLs(urls[[1L]])
NULL



#' @describeIn checkURL Requires a single URL.
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
check_url <-  # nolint
    checkURL



#' @rdname checkURL
#' @export
testURL <- makeTestFunction(checkURL)


#' @rdname checkURL
#' @export
test_url <-  # nolint
    testURL



#' @rdname checkURL
#' @export
assertURL <- makeAssertionFunction(checkURL)



#' @rdname checkURL
#' @export
assert_url <-  # nolint
    assertURL



#' @rdname checkURL
#' @export
expect_url <-  # nolint
    makeExpectationFunction(checkURL)



#' @describeIn checkURL Requires multiple URLs, which is less common.
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
check_urls <-  # nolint
    checkURLs



#' @rdname checkURL
#' @export
testURLs <- makeTestFunction(checkURLs)



#' @rdname checkURL
#' @export
test_urls <-  # nolint
    testURLs



#' @rdname checkURL
#' @export
assertURLs <- makeAssertionFunction(checkURLs)



#' @rdname checkURL
#' @export
assert_urls <-  # nolint
    assertURLs



#' @rdname checkURL
#' @export
expect_urls <-  # nolint
    makeExpectationFunction(checkURLs)
