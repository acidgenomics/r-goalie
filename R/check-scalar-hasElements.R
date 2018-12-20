#' Does the input have elements?
#'
#' @name hasElements
#' @inherit params
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
#' ## Pass ====
#' hasElements("hello", n = 1)
#' hasElements(list(a = 1, b = 2), n = 2)
#'
#' ## Fail ====
#' hasElements(list(), n = 1)
NULL



#' @rdname hasElements
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



# FIXME Note that `""` will return TRUE here.
# Need to make this check stricter...
#' @rdname hasElements
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
            length = gettext("%s has length 0."),
            elements = gettext("%s has 0 elements.")
        )
        return(false(msg, .xname))
    }
    TRUE
}



#' @rdname hasElements
#' @export
hasElements <- function(x, n, .xname = getNameInParent(x)) {
    n <- .useFirst(n)
    .checkN(n)
    nElementsX <- .nElements(x)
    if (nElementsX != n) {
        return(false(
            ngettext(
                nElementsX,
                "%s has %d element, not %d.",
                "%s has %d elements, not %d."
            ),
            .xname,
            nElementsX,
            n
        ))
    }
    TRUE
}



#' @rdname hasElements
#' @export
isOfDimension <- function(x, n, .xname = getNameInParent(x)) {
    dimX <- dim(x)
    if (is.null(n)) {
        if (hasDims(x)) {
            return(false(
                ngettext(
                    n = length(dimX),
                    msg1 = "%s has dimension %s, not NULL.",
                    msg2 = "%s has dimensions %s, not NULL."
                ),
                .xname,
                deparse(dimX)
            ))
        }
        return(TRUE)
    }
    .checkN(n)
    if (!isOfLength(dimX, length(n))) {
        return(false(
            ngettext(
                n = length(dimX),
                msg1 = "%s has %d dimension, not %d.",
                msg2 = "%s has %d dimensions, not %d."
            ),
            .xname,
            length(dimX),
            length(n)
        ))
    }
    differences <- dimX != n
    if (any(differences)) {
        bad <- which(differences)
        return(false(
            ngettext(
                n = length(bad),
                msg1 = "Dimension %s of %s is incorrect.",
                msg2 = "Dimensions %s of %s are incorrect."
            ),
            toString(bad),
            .xname
        ))
    }
    TRUE
}



# TODO Somewhat redundant with `hasLength()`. Work on resolution.
#' @rdname hasElements
#' @export
isOfLength <- function(x, n, .xname = getNameInParent(x)) {
    n <- .useFirst(n)
    .checkN(n)
    lengthX <- length(x)
    if (lengthX != n) {
        return(false("%s has length %d, not %d.", .xname, lengthX, n))
    }
    TRUE
}



# FIXME Using `""` will return TRUE.
# Consider hardening this check.
#' @rdname hasElements
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
            length = gettext("%s has length 0."),
            elements = gettext("%s has 0 elements.")
        )
        return(false(msg, .xname))
    }
    TRUE
}
