#' Assert All Are URL
#'
#' @name assertAllAreURL
#' @inherit params
#'
#' @examples
#' isURL(c(
#'     "http://www.r-project.org",
#'     "https://www.r-project.org",
#'     "ftp://r-project.org",
#'     "r-project.org"
#' ))
#' assertAllAreURL(c(
#'     "https://www.r-project.org",
#'     "ftp://r-project.org"
#' ))
NULL



#' @rdname assertAllAreURL
#' @export
isURL <- function(url) {
    if (
        !is.character(url) ||
        length(url) == 0L
    ) {
        return(FALSE)
    }
    vapply(
        X = url,
        FUN = function(url) {
            grepl("^(http(s)?|ftp)\\://.+", url)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}



#' @rdname assertAllAreURL
#' @export
assertAllAreURL <- function(url) {
    assert_is_character(url)
    assert_is_non_empty(url)
    assert_all_are_true(isURL(url))
}
