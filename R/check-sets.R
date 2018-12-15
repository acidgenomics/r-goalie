#' Set Comparisons
#'
#' @name sets
#' @inherit params
#' @inheritParams assertive.sets::assert_is_superset
#'
#' @examples
#' ## Pass ====
#' areDisjointSets(x = c("a", "b"), y = c("c", "d"))
#' areIntersectingSets(x = c("a", "b"), y = c("b", "c"))
#' areSetEqual(x = c("a", "b"), y = c("b", "a"))
#' isSubset(x = "a", y = c("a", "b"))
#'
#' ## This assert is particularly useful for checking required columns.
#' isSuperset(
#'     x = colnames(datasets::ChickWeight),
#'     y = c("Time", "weight", "Diet")
#' )
#'
#' ## Fail ====
#' areDisjointSets(x = c("a", "b"), y = c("b", "a"))
#' areIntersectingSets(x = c("a", "b"), y = c("c", "d"))
#' areSetEqual(x = c("a", "b"), y = c("b", "c"))
#' isSubset(x = "c", y = c("a", "b"))
#' isSuperset(
#'     x = c("Time", "weight", "Diet"),
#'     y = colnames(datasets::ChickWeight)
#' )
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
    assert(hasLength(x), hasLength(y))
    all(x %in% y)
}



#' @rdname sets
#' @importFrom assertive.sets is_superset
#' @export
isSuperset <- is_superset
