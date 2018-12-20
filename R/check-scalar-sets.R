#' Set comparisons
#'
#' @name sets
#' @inherit params
#'
#' @seealso
#' - `assertive.sets::is_subset()`.
#' - `assertive.sets::is_superset()`.
#' - `assertive.sets::are_disjoint_sets()`.
#' - `assertive.sets::are_intersecting_sets()`.
#' - `assertive.sets::are_set_equal()`.
#'
#' @examples
#' ## Pass ====
#' isSubset(x = "a", y = c("a", "b"))
#'
#' ## This assert is particularly useful for checking required columns.
#' isSuperset(
#'     x = colnames(datasets::ChickWeight),
#'     y = c("Time", "weight", "Diet")
#' )
#'
#' areDisjointSets(x = c("a", "b"), y = c("c", "d"))
#' areIntersectingSets(x = c("a", "b"), y = c("b", "c"))
#' areSetEqual(x = c("a", "b"), y = c("b", "a"))
#'
#' ## Fail ====
#' isSubset(x = "c", y = c("a", "b"))
#' isSuperset(
#'     x = c("Time", "weight", "Diet"),
#'     y = colnames(datasets::ChickWeight)
#' )
#'
#' areDisjointSets(x = c("a", "b"), y = c("b", "a"))
#' areIntersectingSets(x = c("a", "b"), y = c("c", "d"))
#' areSetEqual(x = c("a", "b"), y = c("b", "c"))
NULL



# Assertive has `strictly` mode, which enforces that x,y are not set equal.
#' @rdname sets
#' @export
isSubset <- function(x, y) {
    assert(hasLength(x), hasLength(y))
    all(x %in% y)
}



#' @rdname sets
#' @export
isSuperset <- function(x, y) {
    isSubset(y, x)
}



#' @rdname sets
#' @export
areDisjointSets <- function(
    x,
    y,
    .xname = getNameInParent(x),
    .yname = getNameInParent(y)
) {
    intersect <- intersect(x, y)
    if (length(intersect) > 0L) {
        return(false(
            gettext("%s and %s have common elements: %s."),
            .xname, .yname, toString(intersect, width = 100L)
        ))
    }
    TRUE
}



#' @rdname sets
#' @export
areIntersectingSets <- function(
    x,
    y,
    .xname = getNameInParent(x),
    .yname = getNameInParent(y)
) {
    intersect <- intersect(x, y)
    if (length(intersect) == 0L) {
        return(false(
            gettext("%s and %s have no common elements."),
            .xname, .yname
        ))
    }
    TRUE
}



#' @rdname sets
#' @export
areSetEqual <- function(
    x,
    y,
    .xname = getNameInParent(x),
    .yname = getNameInParent(y)
) {
    x <- unique(x)
    y <- unique(y)
    if (length(x) != length(y)) {
        return(false(
            gettext(
                "%s and %s have different numbers of elements (%d versus %d)."
            ),
            .xname, .yname, length(x), length(y)
        ))
    }
    if (!(ok <- isSubset(x, y))) return(ok)
    if (!(ok <- isSubset(y, x))) return(ok)
    TRUE
}
