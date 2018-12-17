#' Does the input have dimensions?
#'
#' @name hasDims
#' @inherit params
#'
#' @seealso
#' - `assertive.properties::has_dims()`.
#' - `assertive.properties::has_rows()`.
#' - `assertive.properties::has_cols()`.
#' - `assertive.properties::has_dimnames()`.
#' - `assertive.properties::has_colnames()`.
#'
#' @examples
#' ## Pass ====
#' x <- datasets::mtcars
#'
#' hasDims(x)
#' hasRows(x)
#' hasCols(x)
#'
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
#'
#' ## Note that dims don't have to be non-zero, just not NULL.
#' hasDims(data.frame())
#'
#' ## Fail ====
#' x <- data.frame()
#'
#' hasDims(list())
#' hasRows(x)
#' hasCols(x)
#'
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
NULL



#' @rdname hasDims
#' @export
hasDims <- function (x, .xname = getNameInParent(x)) {
    if (is.null(dim(x))) {
        return(false("The dimensions of %s are NULL.", .xname))
    }
    TRUE
}



#' @rdname hasDims
#' @export
hasRows <- function (x, .xname = getNameInParent(x)) {
    nrowx <- nrow(x)
    if (is.null(nrowx)) {
        return(false("The number of rows in %s is NULL.", .xname))
    }
    if (nrowx == 0L) {
        return(false("The number of rows in %s is zero.", .xname))
    }
    TRUE
}



#' @rdname hasDims
#' @export
hasCols <- function (x, .xname = getNameInParent(x)) {
    ncolx <- ncol(x)
    if (is.null(ncolx)) {
        return(false("The number of columns in %s is NULL.", .xname))
    }
    if (ncolx == 0L) {
        return(false("The number of columns in %s is zero.", .xname))
    }
    TRUE
}



#' @rdname hasDims
#' @export
hasDimnames <- function (x, .xname = getNameInParent(x)) {
    dimnamesx <- dimnames(x)
    if (is.null(dimnamesx)) {
        return(false("The dimension names of %s are NULL.", .xname))
    }
    if (!any(nzchar(unlist(dimnamesx, use.names = FALSE)))) {
        return(false("The dimension names of %s are all empty.", .xname))
    }
    TRUE
}



# Documenting hasRownames in a separate Rd file.



#' @rdname hasDims
#' @importFrom assertive.properties has_colnames
#' @export
hasColnames <- function (x, .xname = getNameInParent(x))
{
    colnamesx <- colnames(x)
    if (is.null(colnamesx)) {
        return(false("The column names of %s are NULL.", .xname))
    }
    if (!any(nzchar(colnamesx))) {
        return(false("The column names of %s are all empty.", .xname))
    }
    TRUE
}
