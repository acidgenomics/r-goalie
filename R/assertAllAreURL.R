#' Assert All Are URL
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertAllAreURL(c(
#'     "https://www.r-project.org",
#'     "ftp://r-project.org"
#' ))
assertAllAreURL <- function(object) {
    assert_is_character(object)
    assert_is_non_empty(object)
    assert_all_are_true(isURL(object))
}



#' @rdname assertAllAreURL
#' @export
#' @examples
#' isURL(c(
#'     "http://www.r-project.org",
#'     "https://www.r-project.org",
#'     "ftp://r-project.org",
#'     "r-project.org"
#' ))
isURL <- function(object) {
    if (!is.character(object) || !has_length(object)) {
        return(FALSE)
    }
    vapply(
        X = object,
        FUN = function(object) {
            grepl("^(http(s)?|ftp)\\://.+", object)
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )
}
