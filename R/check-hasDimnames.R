#' Does the input have dimnames?
#'
#' @export
#' @inherit params
#'
#' @seealso
#' - `hasRownames()`.
#' - `assertive.properties::has_dimnames()`.
#' - `assertive.properties::has_colnames()`.
#'
#' @examples
#' ## Pass ====
#' x <- datasets::mtcars
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
#'
#' ## Fail ====
#' x <- data.frame()
#' hasDimnames(x)
#' hasRownames(x)
#' hasColnames(x)
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



# Documenting `hasRownames()` in a separate Rd file because it's complicated.



#' @rdname hasDims
#' @importFrom assertive.properties has_colnames
#' @export
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
