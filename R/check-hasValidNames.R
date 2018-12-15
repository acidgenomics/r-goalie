#' Does the Input Object Have Syntactically Valid Names?
#'
#' @name hasValidNames
#' @inherit params
#'
#' @seealso `validNames`.
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



# Names ========================================================================
.hasValidNames <- function(x) {
    names <- names(x)
    ok <- length(names) > 0L
    if (!isTRUE(ok)) {
        return("Object does not have names")
    }
    ok <- isTRUE(validNames(names))
    if (!isTRUE(ok)) {
        return("Object does not have valid names")
    }
    TRUE
}



#' @rdname hasValidNames
#' @export
hasValidNames <- makeTestFunction(.hasValidNames)




# Dimnames =====================================================================
.hasValidDimnames <- function(x) {
    # Row names.
    if (isTRUE(hasRownames(x))) {
        rownames <- rownames(x)
        ok <- validNames(rownames)
        if (!isTRUE(ok)) {
            return("Row names are invalid")
        }
    }

    # Column names.
    if (isTRUE(hasColnames(x))) {
        colnames <- colnames(x)
        ok <- validNames(colnames)
        if (!isTRUE(ok)) {
            return("Column names are invalid")
        }
    }

    TRUE
}



#' @rdname hasValidNames
#' @export
hasValidDimnames <- makeTestFunction(.hasValidDimnames)
