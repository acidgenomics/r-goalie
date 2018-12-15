# NOTE These functions explicitly call the stringi package, so be sure to
# include that in the imports.



#' Does the String Match a Pattern?
#' @name isPattern
#' @inherit params
#' @inheritParams assertive.strings::is_matching_fixed
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
#' @importFrom assertive.strings is_matching_fixed
#' @export
isMatchingFixed <- is_matching_fixed



#' @rdname isPattern
#' @importFrom assertive.strings is_not_matching_fixed
#' @export
isNotMatchingFixed <- is_not_matching_fixed



#' @rdname isPattern
#' @importFrom assertive.strings is_matching_regex
#' @export
isMatchingRegex <- is_matching_regex



#' @rdname isPattern
#' @importFrom assertive.strings is_not_matching_regex
#' @export
isNotMatchingRegex <- is_not_matching_regex
