#' Does the string match a pattern?
#'
#' @name isMatching
#' @inherit params
#'
#' @seealso
#' - `grepl()`.
#' - `stringi::stri_detect_regex()`.
#' - `stringi::stri_detect_fixed()`.
#' - `stringr::str_detect()`.
#' - `assertive.strings::is_matching_regex()`.
#' - `assertive.strings::is_matching_fixed()`.
#' - `assertive.strings::is_not_matching_regex()`.
#' - `assertive.strings::is_not_matching_fixed()`.
#'
#' @return `logical`.
#'
#' @examples
#' ## TRUE ====
#' isMatchingRegex(x = "foobar", pattern = "^f")
#' isNotMatchingRegex(x = "foobar", pattern = "^b")
#'
#' isMatchingFixed(x = "foobar", pattern = "bar")
#' isNotMatchingFixed(x = "foo", pattern = "bar")
NULL



# vector =======================================================================
#' @rdname isMatching
#' @export
isMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fun = function(x) {
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



#' @rdname isMatching
#' @export
isMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fun = function(x) {
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



#' @rdname isMatching
#' @export
isNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fun = function(x) {
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



#' @rdname isMatching
#' @export
isNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    callAndName(
        fun = function(x) {
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
#' @rdname isMatching
#' @export
allAreMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @rdname isMatching
#' @export
allAreMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @rdname isMatching
#' @export
allAreNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @rdname isMatching
#' @export
allAreNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
