#' Does the input have dimnames?
#'
#' @name check-scalar-hasDimnames
#' @note Updated 2023-09-29.
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
hasDimnames <- function(x) {
    dimnames <- tryCatch(
        expr = dimnames(x),
        error = function(e) {
            e
        }
    )
    if (is(dimnames, "error")) {
        return(false(
            "{.fun %s} command on {.var %s} failed.", "dimnames",
            toCauseName(x)
        ))
    }
    if (is.null(dimnames)) {
        return(false(
            "The dimension names of {.var %s} are {.val %s}.",
            toCauseName(x), "NULL"
        ))
    }
    if (!any(nzchar(unlist(dimnames, use.names = FALSE)))) {
        return(false(
            "The dimension names of {.var %s} are all empty.",
            toCauseName(x)
        ))
    }
    TRUE
}



#' @rdname check-scalar-hasDimnames
#' @export
hasColnames <- function(x) {
    colnames <- tryCatch(
        expr = colnames(x),
        error = function(e) {
            e
        }
    )
    if (is(colnames, "error")) {
        return(false(
            "{.fun %s} command on {.var %s} failed.", "colnames",
            toCauseName(x)
        ))
    }
    if (is.null(colnames)) {
        return(false(
            "The column names of {.var %s} are {.val %s}.",
            toCauseName(x), "NULL"
        ))
    }
    if (!any(nzchar(colnames))) {
        return(false(
            "The column names of {.var %s} are all empty.",
            toCauseName(x)
        ))
    }
    TRUE
}
