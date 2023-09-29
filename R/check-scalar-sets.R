#' Set comparisons
#'
#' @name check-scalar-sets
#' @note Updated 2022-12-14.
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



## Assertive has `strictly` mode, which enforces that x, y are not set equal.

#' @rdname check-scalar-sets
#' @export
isSubset <-
    function(x,
             y,
             .xname = getNameInParent(x),
             .yname = getNameInParent(y)) {
        if (is.null(x)) {
            return(false(gettext("{.var %s} is NULL."), .xname))
        }
        if (is.null(y)) {
            return(false(gettext("{.var %s} is NULL."), .yname))
        }
        ## FIXME Return FALSE on failures here.
        assert(isVectorish(x), isVectorish(y))
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(y)
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
                gettext("{.var %s} has elements not in {.var %s}: %s"),
                .xname, .yname, toString(setdiff, width = 100L)
            ))
        }
        TRUE
    }



## This is essentially an `isSubset()` call with x and y flipped.

#' @rdname check-scalar-sets
#' @export
isSuperset <-
    function(x,
             y,
             .xname = getNameInParent(x),
             .yname = getNameInParent(y)) {
        isSubset(x = y, y = x, .xname = .yname, .yname = .xname)
    }



#' @rdname check-scalar-sets
#' @export
areDisjointSets <-
    function(x,
             y,
             .xname = getNameInParent(x),
             .yname = getNameInParent(y)) {
        if (!is.null(x)) {
            assert(isVectorish(x, .xname = .xname))
        }
        if (!is.null(y)) {
            assert(isVectorish(y, .xname = .yname))
        }
        intersect <- intersect(x, y)
        if (hasLength(intersect)) {
            return(false(
                gettext("{.var %s} and {.var %s} have common elements: %s"),
                .xname, .yname, toString(intersect, width = 100L)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-sets
#' @export
areIntersectingSets <-
    function(x,
             y,
             .xname = getNameInParent(x),
             .yname = getNameInParent(y)) {
        if (!is.null(x)) {
            assert(isVectorish(x, .xname = .xname))
        }
        if (!is.null(y)) {
            assert(isVectorish(y, .xname = .yname))
        }
        intersect <- intersect(x, y)
        if (!hasLength(intersect)) {
            return(false(
                gettext("{.var %s} and {.var %s} have 0 common elements."),
                .xname, .yname
            ))
        }
        TRUE
    }



#' @rdname check-scalar-sets
#' @export
areSetEqual <-
    function(x,
             y,
             .xname = getNameInParent(x),
             .yname = getNameInParent(y)) {
        if (!is.null(x)) {
            assert(isVectorish(x, .xname = .xname))
        }
        if (!is.null(y)) {
            assert(isVectorish(y, .xname = .yname))
        }
        x <- unique(x)
        y <- unique(y)
        if (length(x) != length(y)) {
            return(false(
                gettext(paste(
                    "{.var %s} and {.var %s} have different numbers",
                    "of elements (%d versus %d)."
                )),
                .xname, .yname, length(x), length(y)
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
