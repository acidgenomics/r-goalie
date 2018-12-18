#' Does the input have duplicates?
#'
#' @name hasDuplicates
#' @inherit params
#'
#' @seealso
#' - `assertive.properties::has_duplicates()`.
#' - `assertive.properties::has_no_duplicates()`.
#'
#' @examples
#' ## Pass ====
#' hasDuplicates(c("a", "a"))
#' hasNoDuplicates(c("a", "b"))
#'
#' ## Fail ====
#' hasDuplicates(c("a", "b"))
#' hasNoDuplicates(c("a", "a", "b", "b"))
NULL



#' @rdname hasDuplicates
#' @export
hasDuplicates <- function (x, .xname = getNameInParent(x)) {
    if (!anyDuplicated(x)) {
        return(false(gettext("%s has no duplicates."), .xname))
    }
    TRUE
}



#' @rdname hasDuplicates
#' @export
hasNoDuplicates <- function (x, .xname = getNameInParent(x)) {
    if (anyDuplicated(x)) {
        dupeIndicies <- which(duplicated(x))
        return(false(
            ngettext(
                n = length(dupeIndicies),
                msg1 = "%s has a duplicate at position %s.",
                msg2 = "%s has duplicates at positions %s."
            ),
            .xname,
            toString(dupeIndicies, width = 100L)
        ))
    }
    TRUE
}
