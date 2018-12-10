# TODO Need examples.

#' Set Comparisons
#' @name sets
#' @inherit params
#' @inheritParams assertive.sets::assert_is_superset
NULL



#' @rdname sets
#' @importFrom assertive.sets are_disjoint_sets
#' @export
areDisjointSets <- are_disjoint_sets



#' @rdname sets
#' @importFrom assertive.sets are_intersecting_sets
#' @export
areIntersectingSets <- are_intersecting_sets



#' @rdname sets
#' @importFrom assertive.sets are_set_equal
#' @export
areSetEqual <- are_set_equal



# NOTE: `assertive.sets::is_subset` doesn't error on NULL.
#' @rdname sets
#' @export
isSubset <- function(x, y) {
    all(x %in% y)
}



#' @rdname sets
#' @importFrom assertive.sets is_superset
#' @export
isSuperset <- is_superset
