#' Does the input have dimensions?
#'
#' @name check-scalar-hasDims
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `assertive.properties::has_dims()`.
#' - `assertive.properties::has_rows()`.
#' - `assertive.properties::has_cols()`.
#'
#' @examples
#' ## TRUE ====
#' x <- datasets::mtcars
#' hasDims(x)
#' hasRows(x)
#' hasCols(x)
#'
#' ## Note that dims don't have to be non-zero, just not NULL.
#' hasDims(data.frame())
#'
#' ## FALSE ====
#' x <- data.frame()
#' hasDims(list())
#' hasRows(x)
#' hasCols(x)
NULL



#' @rdname check-scalar-hasDims
#' @export
hasDims <- function(x, .xname = getNameInParent(x)) {
    if (is.null(dim(x))) {
        return(false(
            "The dimensions of {.var %s} are {.val %s}.",
            .xname, "NULL"
        ))
    }
    TRUE
}



#' @rdname check-scalar-hasDims
#' @export
hasRows <- function(x, .xname = getNameInParent(x)) {
    nrowx <- nrow(x)
    if (is.null(nrowx)) {
        return(false(
            "The number of rows in {.var %s} is {.val %s}.",
            .xname, "NULL"
        ))
    }
    if (identical(nrowx, 0L)) {
        return(false("The number of rows in {.var %s} is zero.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-hasDims
#' @export
hasCols <- function(x, .xname = getNameInParent(x)) {
    ncolx <- ncol(x)
    if (is.null(ncolx)) {
        return(false(
            "The number of columns in {.var %s} is {.val %s}.",
            .xname, "NULL"
        ))
    }
    if (identical(ncolx, 0L)) {
        return(false("The number of columns in {.var %s} is zero.", .xname))
    }
    TRUE
}
