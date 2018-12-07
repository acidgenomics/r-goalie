#' Does the Object Have Syntactically Valid Dimnames?
#'
#' @name checkHasValidDimnames
#' @aliases hasValidDimnames
#' @inherit params
#'
#' @examples
#' ## Pass.
#' checkHasValidDimnames(datasets::iris)
#'
#' ## Fail.
#' checkHasValidDimnames(datasets::mtcars)
NULL



#' @rdname checkHasValidDimnames
#' @export
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
check_has_valid_dimnames <- checkHasValidDimnames



#' @rdname checkHasValidDimnames
#' @export
testHasValidDimnames <- makeTestFunction(checkHasValidDimnames)



#' @rdname checkHasValidDimnames
#' @export
test_has_valid_dimnames <- testHasValidDimnames



#' @rdname checkHasValidDimnames
#' @export
assertHasValidDimnames <- makeAssertionFunction(checkHasValidDimnames)



#' @rdname checkHasValidDimnames
#' @export
assert_has_valid_dimnames <- assertHasValidDimnames



#' @rdname checkHasValidDimnames
#' @export
expect_has_valid_dimnames <- makeExpectationFunction(checkHasValidDimnames)
