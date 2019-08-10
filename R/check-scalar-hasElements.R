## Note that `isEmpty()` masks IRanges, GenomicRanges.



#' Does the input have elements?
#'
#' @name check-scalar-hasElements
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' - `assertive.properties::is_empty()`.
#' - `assertive.properties::is_non_empty()`.
#' - `assertive.properties::has_elements()`.
#' - `assertive.properties::is_of_dimension()`.
#' - `assertive.properties::is_of_length()`.
#' - `assertive.properties::is_non_empty()`.
#'
#' @examples
#' ## TRUE ====
#' hasElements("hello", n = 1)
#' hasElements(list(a = 1, b = 2), n = 2)
#'
#' ## FALSE ====
#' hasElements(list(), n = 1)
NULL



#' @rdname check-scalar-hasElements
#' @export
isEmpty <- function(
    x,
    metric = c("length", "elements"),
    .xname = getNameInParent(x)
) {
    metric <- match.arg(metric)
    metricFun <- .getMetric(metric)
    metricFun(x, 0L, .xname)
}



#' @rdname check-scalar-hasElements
#' @export
isNonEmpty <- function(
    x,
    metric = c("length", "elements"),
    .xname = getNameInParent(x)
) {
    metric <- match.arg(metric)
    metricFun <- .getMetric(metric)
    if (metricFun(x, 0L)) {
        msg <- switch(
            EXPR = metric,
            length = gettext("'%s' has length 0."),
            elements = gettext("'%s' has 0 elements.")
        )
        return(false(msg, .xname))
    }
    TRUE
}



#' @rdname check-scalar-hasElements
#' @export
hasElements <- function(x, n, .xname = getNameInParent(x)) {
    assert(is.numeric(n))
    nElementsX <- .nElements(x)
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



#' @rdname check-scalar-hasElements
#' @export
isOfDimension <- function(x, n, .xname = getNameInParent(x)) {
    assert((is.numeric(n) && length(n) == 2L) || is.null(n))
    dimX <- dim(x)
    if (is.null(n)) {
        if (hasDims(x)) {
            return(false(
                ngettext(
                    n = length(dimX),
                    msg1 = "'%s' has dimension %s, not NULL.",
                    msg2 = "'%s' has dimensions %s, not NULL."
                ),
                .xname,
                deparse(dimX)
            ))
        }
        return(TRUE)
    }
    ok <- dimX == n
    if (!all(ok)) {
        notok <- which(!ok)
        return(false(
            ngettext(
                n = length(notok),
                msg1 = "Dimension %s of '%s' is incorrect.",
                msg2 = "Dimensions %s of '%s' are incorrect."
            ),
            toString(notok),
            .xname
        ))
    }
    TRUE
}



#' @rdname check-scalar-hasElements
#' @export
isOfLength <- function(x, n, .xname = getNameInParent(x)) {
    assert(is.numeric(n), length(n) == 1L)
    lengthX <- length(x)
    if (lengthX != n) {
        return(false("'%s' has length %d, not %d.", .xname, lengthX, n))
    }
    TRUE
}



#' @rdname check-scalar-hasElements
#' @export
isNonEmpty <- function(
    x,
    metric = c("length", "elements"),
    .xname = getNameInParent(x)
) {
    metric <- match.arg(metric)
    metricFun <- .getMetric(metric)
    if (metricFun(x, 0L)) {
        msg <- switch(
            EXPR = metric,
            length = gettext("'%s' has length 0."),
            elements = gettext("'%s' has 0 elements.")
        )
        return(false(msg, .xname))
    }
    TRUE
}
