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



#' @describeIn checkURL snake alias.
#' @export
check_url <-  # nolint
    checkURL



#' @rdname checkURL
#' @export
testURL <- makeTestFunction(checkURL)


#' @describeIn checkURL snake alias.
#' @export
test_url <-  # nolint
    testURL



#' @rdname checkURL
#' @export
assertURL <- makeAssertionFunction(checkURL)



#' @describeIn checkURL snake alias.
#' @export
assert_url <-  # nolint
    assertURL



#' @rdname checkURL
#' @export
expect_url <-  # nolint
    makeExpectationFunction(checkURL)



#' @describeIn checkURL Supports multiple URLs.
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



#' @describeIn checkURL snake alias.
#' @export
check_urls <-  # nolint
    checkURLs



#' @rdname checkURL
#' @export
testURLs <- makeTestFunction(checkURLs)



#' @describeIn checkURL snake alias.
#' @export
test_urls <-  # nolint
    testURLs



#' @rdname checkURL
#' @export
assertURLs <- makeAssertionFunction(checkURLs)



#' @describeIn checkURL snake alias.
#' @export
assert_urls <-  # nolint
    assertURLs



#' @rdname checkURL
#' @export
expect_urls <-  # nolint
    makeExpectationFunction(checkURLs)
