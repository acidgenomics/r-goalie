#' Does the Input Have Duplicates?
#'
#' @name hasDuplicates
#' @importFrom assertive.properties has_duplicates has_no_duplicates
#' @inherit params
#'
#' @seealso
#' - `assertive.properties::has_duplicates()`.
#' - `assertive.properties::has_no_duplicates()`.
#'
#' @examples
#' hasDuplicates(c("a", "a"))
#' hasNoDuplicates(c("a", "b"))
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
                length(dupeIndicies),
                "%s has a duplicate at position %s.",
                "%s has duplicates at positions %s."
            ),
            .xname,
            toString(dupeIndicies, width = 100L)
        ))
    }
    TRUE
}
