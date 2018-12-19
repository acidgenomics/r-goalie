# TODO Enforce scalar here?



#' Does the string match a pattern?
#'
#' @name grepl
#' @inherit params
#'
#' @seealso
#' - `grepl()`.
#' - `stringi::stri_detect_fixed()`.
#' - `stringi::stri_detect_regex()`.
#' - `stringr::str_detect()`.
#' - `assertive.strings::is_matching_fixed()`.
#' - `assertive.strings::is_matching_regex()`.
#' - `assertive.strings::is_not_matching_fixed()`.
#' - `assertive.strings::is_not_matching_regex()`.
#'
#' @return `logical`.
#'
#' @examples
#' isMatchingFixed(x = "foobar", pattern = "bar")
#' isNotMatchingFixed(x = "foo", pattern = "bar")
#' isMatchingRegex(x = "foobar", pattern = "^f")
#' isNotMatchingRegex(x = "foobar", pattern = "^b")
NULL



#' @rdname grepl
#' @export
isMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fn = function(x) {
            ok <- grepl(
                pattern = pattern,
                x = x,
                ignore.case = FALSE,
                fixed = TRUE
            )
            setCause(ok, false = gettextf("does not match '%s'", pattern))
        },
        x = x
    )
}



#' @rdname grepl
#' @export
isMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fn = function(x) {
            ok <- grepl(
                pattern = pattern,
                x = x,
                ignore.case = FALSE,
                fixed = FALSE
            )
            setCause(ok, false = gettextf("does not match '%s'", pattern))
        },
        x = x
    )
}



#' @rdname grepl
#' @export
isNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fn = function(x) {
            ok <- !grepl(
                pattern = pattern,
                x = x,
                ignore.case = FALSE,
                fixed = TRUE
            )
            setCause(ok, false = gettextf("matches '%s'", pattern))
        },
        x = x
    )
}



#' @rdname grepl
#' @export
isNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fn = function(x) {
            ok <- !grepl(
                pattern = pattern,
                x = x,
                ignore.case = FALSE,
                fixed = FALSE
            )
            setCause(ok, false = gettextf("matches '%s'", pattern))
        },
        x = x
    )
}
