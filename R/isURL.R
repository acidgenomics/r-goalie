#' Is URL?
#'
#' @name isURL
#' @inherit params
#'
#' @examples
#' urls <- c("https://www.r-project.org", "ftp://r-project.org")
#'
#' ## Requires scalar.
#' isURL(urls[[1L]])
#'
#' ## Parameterized.
#' areURLs(urls)
NULL



# isURL ========================================================================
#' @rdname isURL
#' @export
isURL <- function(x) {
    assert_that(is_string(x))
    vapply(
        X = x,
        FUN = function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}

.msg.isURL <- function(x) {
    paste0(
        x, " does not contain a URL.\n",
        "URLs must begin with `http(s)` or `ftp` and contain `://`."
    )
}

on_failure(isURL) <- function(call, env) {
    .msg.isURL(x = deparse(call[["x"]]))
}

#' @rdname isURL
#' @export
assertIsURL <- function(x) {
    assert_that(
        isURL(x),
        msg = .msg.isURL(x = deparse(substitute(x)))
    )
}



# areURLs ======================================================================
#' @rdname isURL
#' @export
areURLs <- function(x) {
    all(vapply(
        X = x,
        FUN = isURL,
        FUN.VALUE = logical(1L)
    ))
}

on_failure(areURLs) <- function(call, env) {
    .msg.isURL(x = deparse(call[["x"]]))
}

#' @rdname isURL
#' @export
assertAreURLs <- function(x) {
    assert_that(
        areURLs(x),
        msg = .msg.isURL(x = deparse(substitute(x)))
    )
}
