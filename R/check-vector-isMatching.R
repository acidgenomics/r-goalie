#' Does the string match a pattern?
#'
#' @name check-vector-isMatching
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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



## grep matching that dynamically handles S4 objects, if necessary.
## Alternatively, could check directly for Rle and decode here instead.
## Updated 2023-10-02.
.grepl <- function(x, ...) {
    if (!is.atomic(x)) {
        return(FALSE)
    }
    if (isS4(x)) {
        requireNamespaces("S4Vectors")
        grepl <- S4Vectors::grepl
    }
    ok <- grepl(x = x, ...)
    if (isS4(x)) {
        ok <- as.logical(ok)
    }
    ok
}



## Vector ======================================================================

#' @describeIn check-vector-isMatching Vectorized.
#' @export
isMatchingFixed <- function(x, pattern) {
    ok <- .grepl(
        pattern = pattern,
        x = x,
        ignore.case = FALSE,
        fixed = TRUE
    )
    setCause(ok, false = sprintf("doesn't match {.var %s}", pattern))
}



#' @describeIn check-vector-isMatching Vectorized.
#' @export
isMatchingRegex <- function(x, pattern) {
    ok <- .grepl(
        pattern = pattern,
        x = x,
        ignore.case = FALSE,
        fixed = FALSE
    )
    setCause(ok, false = sprintf("doesn't match {.var %s}", pattern))
}



#' @describeIn check-vector-isMatching Vectorized.
#' @export
isNotMatchingFixed <- function(x, pattern) {
    ok <- !.grepl(
        pattern = pattern,
        x = x,
        ignore.case = FALSE,
        fixed = TRUE
    )
    setCause(ok, false = sprintf("matches {.var %s}", pattern))
}



#' @describeIn check-vector-isMatching Vectorized.
#' @export
isNotMatchingRegex <- function(x, pattern) {
    ok <- !.grepl(
        pattern = pattern,
        x = x,
        ignore.case = FALSE,
        fixed = FALSE
    )
    setCause(ok, false = sprintf("matches {.var %s}", pattern))
}



## Scalar ======================================================================

#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreMatchingFixed <- function(x, pattern) {
    ok <- isMatchingFixed(x = x, pattern = pattern)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreMatchingRegex <- function(x, pattern) {
    ok <- isMatchingRegex(x = x, pattern = pattern)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreNotMatchingFixed <- function(x, pattern) {
    ok <- isNotMatchingFixed(x = x, pattern = pattern)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreNotMatchingRegex <- function(x, pattern) {
    ok <- isNotMatchingRegex(x = x, pattern = pattern)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
