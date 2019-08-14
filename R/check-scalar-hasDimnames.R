## Documenting `hasRownames()` in a separate Rd file because it's complicated.



#' Does the input have dimnames?
#'
#' @name check-scalar-hasDimnames
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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



#' @rdname check-scalar-hasDimnames
#' @export
hasDimnames <- function(x, .xname = getNameInParent(x)) {
    dimnames <- tryCatch(
        expr = dimnames(x),
        error = function(e) e
    )
    if (is(dimnames, "error")) {
        false("'dimnames()' command on '%s' failed.", .xname)  # nocov
    } else if (is.null(dimnames)) {
        false("The dimension names of '%s' are NULL.", .xname)  # nocov
    } else if (!any(nzchar(unlist(dimnames, use.names = FALSE)))) {
        false("The dimension names of '%s' are all empty.", .xname)
    } else {
        TRUE
    }
}



#' @rdname check-scalar-hasDimnames
#' @export
hasColnames <- function(x, .xname = getNameInParent(x)) {
    colnames <- tryCatch(
        expr = colnames(x),
        error = function(e) e
    )
    if (is(colnames, "error")) {
        false("'colnames()' command on '%s' failed.", .xname)  # nocov
    } else if (is.null(colnames)) {
        false("The column names of '%s' are NULL.", .xname)  # nocov
    } else if (!any(nzchar(colnames))) {
        false("The column names of '%s' are all empty.", .xname)
    } else {
        TRUE
    }
}
