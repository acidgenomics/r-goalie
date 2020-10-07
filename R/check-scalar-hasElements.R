#' Does the input have elements?
#'
#' @name check-scalar-hasElements
#' @note Updated 2019-10-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `prod()`, which returns the product of all values in its arguments. This
#'   is called internally to check the number of elements.
#' - `assertive.properties::has_elements()`.
#' - `assertive.properties::is_of_dimension()`.
#' - `assertive.properties:::n_elements()`.
#'
#' @examples
#' ## TRUE ====
#' hasElements("hello")
#' hasElements("hello", n = 1)
#' hasElements(list(a = 1, b = 2), n = 2)
#'
#' ## FALSE ====
#' hasElements(NULL)
#' hasElements(list(), n = 1)
NULL



#' @rdname check-scalar-hasElements
#' @export
hasElements <- function(x, n = NULL, .xname = getNameInParent(x)) {
    nElementsX <- nElements(x)
    if (is.null(n)) {
        if (identical(nElementsX, 0L)) {
            return(false("'%s' has 0 elements.", .xname))
        } else {
            return(TRUE)  # nocov
        }
    }
    ## We're using `prod()` here to check vector n input (e.g. `c(nrow, ncol)`).
    nElementsN <- prod(n)
    if (nElementsX != nElementsN) {
        return(false(
            ngettext(
                nElementsX,
                "'%s' has %d element, not %d.",
                "'%s' has %d elements, not %d."
            ),
            .xname,
            nElementsX,
            nElementsN
        ))
    }
    TRUE
}



#' @describeIn check-scalar-hasElements Return the number of elements in object.
#' @export
nElements <- function(x) {
    if (is.recursive(x)) {
        sum(vapply(x, nElements, integer(1L)))
    }
    else {
        as.integer(prod(.dim(x)))
    }
}
