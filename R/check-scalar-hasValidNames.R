#' Does the input object have syntactically valid names?
#'
#' @name check-scalar-hasValidNames
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso `validNames()`.
#'
#' @examples
#' ## TRUE ====
#' x <- list(a = 1, b = 2)
#' names(x)
#' hasValidNames(x)
#'
#' x <- datasets::iris
#' lapply(dimnames(x), head)
#' hasValidDimnames(x)
#'
#' ## FALSE ====
#' x <- list(
#'     `1`       = 1,  # can't start with number
#'     `foo bar` = 2,  # no spaces
#'     `foo-bar` = 3   # no hyphens
#' )
#' print(x)
#' hasValidNames(x)
#'
#' x <- datasets::mtcars
#' # Note the spaces in the row names here.
#' lapply(dimnames(x), head)
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
        return(false("`names()` command on %s failed.", .xname))  # nocov
    }
    ok <- length(names) > 0L
    if (!isTRUE(ok)) {
        return(false("%s does not have names.", .xname))
    }
    ok <- isTRUE(validNames(names))
    if (!isTRUE(ok)) {
        return(false("%s does not have valid names.", .xname))
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
        return(false("`dimnames()` command on %s failed.", .xname))  # nocov
    }

    ## Row names.
    if (isTRUE(hasRownames(x))) {
        rownames <- rownames(x)
        ok <- validNames(rownames)
        if (!isTRUE(ok)) {
            return(false("%s has invalid row names.", .xname))
        }
    }

    ## Column names.
    if (isTRUE(hasColnames(x))) {
        colnames <- colnames(x)
        ok <- validNames(colnames)
        if (!isTRUE(ok)) {
            return(false("%s has invalid column names.", .xname))
        }
    }

    TRUE
}
