# Documenting `hasRownames()` in a separate Rd file because it's complicated.

#' Does the input have dimnames?
#'
#' @name hasDimnames
#' @export
#'
#' @inherit params
#'
#' @seealso
#' - `hasRownames()`.
#' - `assertive.properties::has_dimnames()`.
#' - `assertive.properties::has_colnames()`.
#'
#' @examples
#' ## TRUE ====
#' x <- datasets::mtcars
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
#'
#' ## FALSE ====
#' x <- data.frame()
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
NULL

#' @rdname hasDimnames
#' @export
# Updated 2019-07-15.
hasDimnames <- function(x, .xname = getNameInParent(x)) {
    dimnamesx <- dimnames(x)
    if (is.null(dimnamesx)) {
        return(false("The dimension names of %s are NULL.", .xname))
    }
    if (!any(nzchar(unlist(dimnamesx, use.names = FALSE)))) {
        return(false("The dimension names of %s are all empty.", .xname))
    }
    TRUE
}



#' @rdname hasDimnames
#' @export
# Updated 2019-07-15.
hasColnames <- function(x, .xname = getNameInParent(x)) {
    colnamesx <- colnames(x)
    if (is.null(colnamesx)) {
        return(false("The column names of %s are NULL.", .xname))
    }
    if (!any(nzchar(colnamesx))) {
        return(false("The column names of %s are all empty.", .xname))
    }
    TRUE
}
