#' Does the input object have syntactically valid names?
#'
#' @name hasValidNames
#' @inherit params
#'
#' @seealso `validNames()`.
#'
#' @examples
#' ## Pass ====
#' x <- list(a = 1, b = 2)
#' names(x)
#' hasValidNames(x)
#'
#' x <- datasets::iris
#' lapply(dimnames(x), head)
#' hasValidDimnames(x)
#'
#' ## Fail ====
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



#' @rdname hasValidNames
#' @export
hasValidNames <- function(x, .xname = getNameInParent(x)) {
    names <- names(x)
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



#' @rdname hasValidNames
#' @export
hasValidDimnames <- function(x, .xname = getNameInParent(x)) {
    # Row names.
    if (isTRUE(hasRownames(x))) {
        rownames <- rownames(x)
        ok <- validNames(rownames)
        if (!isTRUE(ok)) {
            return(false("%s has invalid row names.", .xname))
        }
    }

    # Column names.
    if (isTRUE(hasColnames(x))) {
        colnames <- colnames(x)
        ok <- validNames(colnames)
        if (!isTRUE(ok)) {
            return(false("%s has invalid column names.", .xname))
        }
    }

    TRUE
}