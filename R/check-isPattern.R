# TODO Enforce scalar here?



#' Does the string match a pattern?
#'
#' @name isPattern
#' @inherit params
#'
#' @seealso
#' - `assertive.strings::is_matching_fixed()`.
#' - `assertive.strings::is_not_matching_fixed()`.
#' - `assertive.strings::is_matching_regex()`.
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



#' @rdname isPattern
#' @export
isMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- stri_detect_fixed(x, pattern)
    if (!isTRUE(ok)) {
        return(false("‘%s’ pattern does not match in %s.", pattern, .xname))
    }
    TRUE
}



#' @rdname isPattern
#' @export
isNotMatchingFixed <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- !stri_detect_fixed(x, pattern)
    if (!isTRUE(ok)) {
        return(false("‘%s’ pattern matches in %s.", pattern, .xname))
    }
    TRUE
}



#' @rdname isPattern
#' @export
isMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- stri_detect_regex(x, pattern)
    if (!isTRUE(ok)) {
        return(false("‘%s’ pattern does not match in %s.", pattern, .xname))
    }
    TRUE
}



#' @rdname isPattern
#' @export
isNotMatchingRegex <- function(x, pattern, .xname = getNameInParent(x)) {
    ok <- !stri_detect_regex(x, pattern)
    if (!isTRUE(ok)) {
        return(false("‘%s’ pattern matches in %s.", pattern, .xname))
    }
    TRUE
}
