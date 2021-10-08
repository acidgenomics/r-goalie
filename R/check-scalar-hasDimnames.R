## Documenting `hasRownames()` in a separate Rd file because it's complicated.



#' Does the input have dimnames?
#'
#' @name check-scalar-hasDimnames
#' @note Updated 2021-10-07.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
        false("{.fun %s} command on {.var %s} failed.", "dimnames", .xname)
    } else if (is.null(dimnames)) {
        false(
            "The dimension names of {.var %s} are {.val %s}.",
            .xname, "NULL"
        )
    } else if (!any(nzchar(unlist(dimnames, use.names = FALSE)))) {
        false("The dimension names of {.var %s} are all empty.", .xname)
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
        false("{.fun %s} command on {.var %s} failed.", "colnames", .xname)
    } else if (is.null(colnames)) {
        false("The column names of {.var %s} are {.val %s}.", .xname, "NULL")
    } else if (!any(nzchar(colnames))) {
        false("The column names of {.var %s} are all empty.", .xname)
    } else {
        TRUE
    }
}
