#' Assert All Are URL
#'
#' @name assertAllAreURL
#' @inherit params
#'
#' @examples
#' assertAllAreURL(c(
#'     "https://www.r-project.org",
#'     "ftp://r-project.org"
#' ))
NULL



#' @rdname assertAllAreURL
#' @export
assertAllAreURL <- function(x) {
    assert_that(all(isURL(x)))
}



#' @rdname assertAllAreURL
#' @export
isURL <- function(x) {
    assert_that(
        is.character(x),
        length(x) > 0L
    )
    vapply(
        X = x,
        FUN = function(x) {
            grepl("^(http(s)?|ftp)\\://.+", x)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}

on_failure(isURL) <- function(call, env) {
    paste0(
        deparse(call$x), " is not a URL.\n",
        "URLs must begin with `http(s)` or `ftp` and contain `://`."
    )
}
