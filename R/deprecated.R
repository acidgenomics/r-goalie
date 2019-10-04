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
## Soft deprecated in favor of `hasLength()`.
## Note that this function is defined in S4Vectors, and we don't want to mask.
#' @rdname deprecated
#' @export
isEmpty <- function(x, .xname = getNameInParent(x)) {
    hasLength(x = x, n = 0L, .xname = .xname)
}

## Soft deprecated in favor of `hasLength()` or `hasElements()` instead.
#' @rdname deprecated
#' @export
isNonEmpty <- function(x, .xname = getNameInParent(x)) {
    hasLength(x = x, n = NULL, .xname = .xname)
}



## nolint end
## nocov end
