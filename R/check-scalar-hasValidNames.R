#' Does the input object have syntactically valid names?
#'
#' @name check-scalar-hasValidNames
#' @note Updated 2022-05-13.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `validNames()`.
#'
#' @examples
#' ## TRUE ====
#' x <- list("a" = 1L, "b" = 2L)
#' names(x)
#' hasValidNames(x)
#'
#' x <- datasets::iris
#' lapply(X = dimnames(x), FUN = head)
#' hasValidDimnames(x)
#'
#' ## FALSE ====
#' x <- list(
#'     "1" = 1, # can't start with number
#'     "foo bar" = 2, # no spaces
#'     "foo-bar" = 3 # no hyphens
#' )
#' print(x)
#' hasValidNames(x)
#'
#' x <- datasets::mtcars
#' # Note the spaces in the row names here.
#' lapply(X = dimnames(x), FUN = head)
#' hasValidDimnames(x)
NULL



#' @rdname check-scalar-hasValidNames
#' @export
hasValidNames <- function(x, .xname = getNameInParent(x)) {
    names <- tryCatch(
        expr = names(x),
        error = function(e) e
    )
    if (is(names, "error")) {
        return(false(
            "{.fun %s} command on {.var %s} failed.",
            "names", .xname
        ))
    }
    ok <- length(names) > 0L
    if (!isTRUE(ok)) {
        return(false("{.var %s} doesn't have names.", .xname))
    }
    ok <- validNames(names, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}



#' @rdname check-scalar-hasValidNames
#' @export
hasValidDimnames <- function(x, .xname = getNameInParent(x)) {
    ## Check for `dimnames()` failure.
    dimnames <- tryCatch(
        expr = dimnames(x),
        error = function(e) e
    )
    if (is(dimnames, "error")) {
        return(false(
            "{.fun %s} command on {.var %s} failed.",
            "dimnames", .xname
        ))
    }
    ## Row names.
    if (isTRUE(hasRownames(x))) {
        rownames <- rownames(x)
        ok <- validNames(rownames, .xname = .xname)
        if (!isTRUE(ok)) {
            return(ok)
        }
    }
    ## Column names.
    if (isTRUE(hasColnames(x))) {
        colnames <- colnames(x)
        ok <- validNames(colnames, .xname = .xname)
        if (!isTRUE(ok)) {
            return(ok)
        }
    }
    TRUE
}
