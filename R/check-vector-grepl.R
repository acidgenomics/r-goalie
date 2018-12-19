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



# vector =======================================================================
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



# scalar =======================================================================
#' @rdname grepl
#' @export
allAreMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(ok)
    TRUE
}



#' @rdname grepl
#' @export
allAreMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(ok)
    TRUE
}



#' @rdname grepl
#' @export
allAreNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(ok)
    TRUE
}



#' @rdname grepl
#' @export
allAreNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(ok)
    TRUE
}
