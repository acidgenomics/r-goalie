#' Does the input have dimensions?
#'
#' @name hasDims
#' @inherit params
#'
#' @seealso
#' - `assertive.properties::has_dims()`.
#' - `assertive.properties::has_rows()`.
#' - `assertive.properties::has_cols()`.
#'
#' @examples
#' ## Pass ====
#' x <- datasets::mtcars
#' hasDims(x)
#' hasRows(x)
#' hasCols(x)
#'
#' ## Note that dims don't have to be non-zero, just not NULL.
#' hasDims(data.frame())
#'
#' ## Fail ====
#' x <- data.frame()
#' hasDims(list())
#' hasRows(x)
#' hasCols(x)
NULL



#' @rdname hasDims
#' @export
hasDims <- function (x) {
    xname = getNameInParent(x)
    if (is.null(dim(x))) {
        return(false("The dimensions of %s are NULL.", xname))
    }
    TRUE
}



#' @rdname hasDims
#' @export
hasRows <- function (x) {
    xname = getNameInParent(x)
    nrowx <- nrow(x)
    if (is.null(nrowx)) {
        return(false("The number of rows in %s is NULL.", xname))
    }
    if (nrowx == 0L) {
        return(false("The number of rows in %s is zero.", xname))
    }
    TRUE
}



#' @rdname hasDims
#' @export
hasCols <- function (x) {
    xname = getNameInParent(x)
    ncolx <- ncol(x)
    if (is.null(ncolx)) {
        return(false("The number of columns in %s is NULL.", xname))
    }
    if (ncolx == 0L) {
        return(false("The number of columns in %s is zero.", xname))
    }
    TRUE
}
