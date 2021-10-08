#' Does the input have duplicates?
#'
#' @name check-scalar-hasDuplicates
#' @note Updated 2021-10-07.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `assertive.properties::has_duplicates()`.
#' - `assertive.properties::has_no_duplicates()`.
#'
#' @examples
#' ## TRUE ====
#' hasDuplicates(c("a", "a"))
#' hasNoDuplicates(c("a", "b"))
#'
#' ## FALSE ====
#' hasDuplicates(c("a", "b"))
#' hasNoDuplicates(c("a", "a", "b", "b"))
NULL



#' @rdname check-scalar-hasDuplicates
#' @export
hasDuplicates <- function(x, .xname = getNameInParent(x)) {
    if (!anyDuplicated(x)) {
        return(false("{.var %s} has no duplicates.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-hasDuplicates
#' @export
hasNoDuplicates <- function(x, .xname = getNameInParent(x)) {
    if (anyDuplicated(x)) {
        dupeIndicies <- which(duplicated(x))
        return(false(
            ngettext(
                n = length(dupeIndicies),
                msg1 = "{.var %s} has a duplicate at position %s.",
                msg2 = "{.var %s} has duplicates at positions %s."
            ),
            .xname,
            toString(dupeIndicies, width = 100L)
        ))
    }
    TRUE
}
