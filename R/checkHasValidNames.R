#' Does the Object Have Syntactically Valid Names?
#'
#' @name checkHasValidNames
#' @aliases hasValidNames
#' @inherit params
#'
#' @seealso `validNames()`.
#'
#' @examples
#' checkHasValidNames(datasets::mtcars)
NULL



#' @rdname checkHasValidNames
#' @export
checkHasValidNames <- function(x) {
    names <- names(x)
    ok <- has_length(names)
    if (!ok) {
        return("Object does not have names")
    }
    ok <- isTRUE(validNames(names))
    if (!ok) {
        return("Object does not have valid names")
    }
    TRUE
}



#' @rdname checkHasValidNames
#' @export
check_has_valid_names <- checkHasValidNames



#' @rdname checkHasValidNames
#' @export
testHasValidNames <- makeTestFunction(checkHasValidNames)



#' @rdname checkHasValidNames
#' @export
test_has_valid_names <- testHasValidNames



#' @rdname checkHasValidNames
#' @export
assertHasValidNames <- makeAssertionFunction(checkHasValidNames)



#' @rdname checkHasValidNames
#' @export
assert_has_valid_names <- assertHasValidNames



#' @rdname checkHasValidNames
#' @export
expect_has_valid_names <- makeExpectationFunction(checkHasValidNames)
