## nocov start
## nolint start



#' @name defunct
#' @inherit AcidRoxygen::defunct description examples return seealso title
#' @inheritParams AcidRoxygen::params
#' @keywords internal
NULL



## v0.2.1 =======================================================================
#' @rdname defunct
#' @export
containsAlpha <- function(...) {
    .Defunct("isAlpha")
}

#' @rdname defunct
#' @export
containsHeaderLevel <- function(...) {
    .Defunct("isHeaderLevel")
}

#' @rdname defunct
#' @export
containsHexColors <- function(...) {
    .Defunct("allAreHexColors")
}

#' @rdname defunct
#' @export
containsURL <- function(...) {
    .Defunct("isURL")
}

#' @rdname defunct
#' @export
containsAURL <- function(...) {
    .Defunct("isAURL")
}



## v0.3.8 ======================================================================
#' @rdname defunct
#' @export
isOfLength <- function(...) {
    .Defunct("hasLength")
}



## v0.3.9 ======================================================================
## These functions are defined in S4Vectors, and we don't want to mask.
#' @rdname defunct
#' @export
isEmpty <- function(x, .xname = getNameInParent(x)) {
    .Defunct("hasLength")
}

## Soft deprecated in favor of `hasLength()` or `hasElements()` instead.
#' @rdname defunct
#' @export
isNonEmpty <- function(x, .xname = getNameInParent(x)) {
    .Defunct("hasLength")
}



## nolint end
## nocov end
