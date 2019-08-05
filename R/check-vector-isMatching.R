#' Does the string match a pattern?
#'
#' @name check-vector-isMatching
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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



## Vector ======================================================================
#' @describeIn check-vector-isMatching Vectorized.
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



#' @describeIn check-vector-isMatching Vectorized.
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



#' @describeIn check-vector-isMatching Vectorized.
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



#' @describeIn check-vector-isMatching Vectorized.
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



## Scalar ======================================================================
#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingFixed(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- isNotMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
