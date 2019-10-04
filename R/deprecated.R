## nocov start
## nolint start



#' @name defunct
#' @inherit acidroxygen::defunct description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



## v0.2.1 =======================================================================
#' @rdname deprecated
#' @export
containsAlpha <- appendToBody(
    fun = isAlpha,
    values = quote(.Deprecated("isAlpha"))
)

#' @rdname deprecated
#' @export
containsHeaderLevel <- appendToBody(
    fun = isHeaderLevel,
    values = quote(.Deprecated("isHeaderLevel"))
)

#' @rdname deprecated
#' @export
containsHexColors <- appendToBody(
    fun = allAreHexColors,
    values = quote(.Deprecated("allAreHexColors"))
)

#' @rdname deprecated
#' @export
containsURL <- isURL

#' @rdname deprecated
#' @export
containsAURL <- isAURL



## v0.3.8 ======================================================================
#' @rdname deprecated
#' @export
isOfLength <- function(...) {
    .Deprecated("hasLength")
    hasLength(...)
}



## v0.3.9 ======================================================================
## Soft deprecated. Defined in S4Vectors.
#' @rdname deprecated
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

## Soft deprecated in favor of `hasLength()` or `hasElements()` instead.
#' @rdname deprecated
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



## nolint end
## nocov end
