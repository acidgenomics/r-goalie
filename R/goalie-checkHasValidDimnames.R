#' Does the Object Have Syntactically Valid Dimnames?
#'
#' @name checkHasValidDimnames
#' @aliases hasValidDimnames has_valid_dimnames
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' x <- datasets::iris
#' lapply(dimnames(x), head)
#' checkHasValidDimnames(x)
#'
#' ## Fail ====
#' x <- datasets::mtcars
#' # Note the spaces in the row names here.
#' lapply(dimnames(x), head)
#' checkHasValidDimnames(x)
checkHasValidDimnames <- function(x) {
    # Row names.
    if (isTRUE(testHasRownames(x))) {
        rownames <- rownames(x)
        ok <- validNames(rownames)
        if (!ok) {
            return("Row names are invalid")
        }
    }

    # Column names.
    if (isTRUE(testHasColnames(x))) {
        colnames <- colnames(x)
        ok <- validNames(colnames)
        if (!ok) {
            return("Column names are invalid")
        }
    }

    TRUE
}



#' @rdname checkHasValidDimnames
#' @export
check_has_valid_dimnames <- checkHasValidDimnames  # nolint



#' @rdname checkHasValidDimnames
#' @export
testHasValidDimnames <- makeTestFunction(checkHasValidDimnames)



#' @rdname checkHasValidDimnames
#' @export
test_has_valid_dimnames <- testHasValidDimnames  # nolint



#' @rdname checkHasValidDimnames
#' @export
assertHasValidDimnames <- makeAssertionFunction(checkHasValidDimnames)



#' @rdname checkHasValidDimnames
#' @export
assert_has_valid_dimnames <- assertHasValidDimnames  # nolint



#' @rdname checkHasValidDimnames
#' @export
expect_has_valid_dimnames <-  # nolint
    makeExpectationFunction(checkHasValidDimnames)
