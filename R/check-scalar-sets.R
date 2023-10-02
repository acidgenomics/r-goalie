#' Set comparisons
#'
#' @name check-scalar-sets
#' @note Updated 2023-10-02.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `assertive.sets::is_subset()`.
#' - `assertive.sets::is_superset()`.
#' - `assertive.sets::are_disjoint_sets()`.
#' - `assertive.sets::are_intersecting_sets()`.
#' - `assertive.sets::are_set_equal()`.
#'
#' @examples
#' ## TRUE ====
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
#' ## FALSE ====
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



#' @rdname check-scalar-sets
#' @export
isSubset <-
    function(x, y) {
        ok <- isVectorish(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        if (isS4(x) || isS4(y)) {
            requireNamespaces("S4Vectors")
            `%in%` <- S4Vectors::`%in%` # nolint
        }
        ok <- all(x %in% y)
        if (!isTRUE(ok)) {
            setdiff <- setdiff(x, y)
            return(false(
                "{.var %s} has elements not in {.var %s}: %s",
                toCauseName(x),
                toCauseName(y),
                toString(setdiff, width = 100L)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-sets
#' @export
isSuperset <-
    function(x, y) {
        isSubset(x = y, y = x)
    }



#' @rdname check-scalar-sets
#' @export
areDisjointSets <-
    function(x, y) {
        ok <- isVectorish(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        int <- intersect(x, y)
        if (hasLength(int)) {
            return(false(
                "{.var %s} and {.var %s} have common elements: %s",
                toCauseName(x), toCauseName(y),
                toString(int, width = 100L)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-sets
#' @export
areIntersectingSets <-
    function(x, y) {
        ok <- isVectorish(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        int <- intersect(x, y)
        if (!hasLength(int)) {
            return(false(
                "{.var %s} and {.var %s} have 0 common elements.",
                toCauseName(x), toCauseName(y)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-sets
#' @export
areSetEqual <-
    function(x, y) {
        ok <- isVectorish(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        x <- unique(x)
        y <- unique(y)
        ok <- length(x) != length(y)
        if (!isTRUE(ok)) {
            return(false(
                paste(
                    "{.var %s} and {.var %s} have different numbers",
                    "of elements (%d versus %d)."
                ),
                toCauseName(x), toCauseName(y),
                length(x), length(y)
            ))
        }
        ok <- isSubset(x, y)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isSubset(y, x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
