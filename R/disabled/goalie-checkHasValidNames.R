#' Does the Object Have Syntactically Valid Names?
#'
#' @name checkHasValidNames
#' @aliases hasValidNames has_valid_names
#' @inherit params
#' @export
#'
#' @seealso `validNames()`.
#'
#' @examples
#' ## Pass ====
#' x <- list(a = 1, b = 2)
#' checkHasValidNames(x)
#'
#' ## Fail ====
#' x <- list(
#'     `1`       = 1,  # can't start with number
#'     `foo bar` = 2,  # no spaces
#'     `foo-bar` = 3   # no hyphens
#' )
#' print(x)
#' checkHasValidNames(x)
checkHasValidNames <- function(x) {
    names <- names(x)
    ok <- length(names) > 0L
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
check_has_valid_names <- checkHasValidNames  # nolint



#' @rdname checkHasValidNames
#' @export
testHasValidNames <- makeTestFunction(checkHasValidNames)



#' @rdname checkHasValidNames
#' @export
test_has_valid_names <- testHasValidNames  # nolint



#' @rdname checkHasValidNames
#' @export
assertHasValidNames <- makeAssertionFunction(checkHasValidNames)



#' @rdname checkHasValidNames
#' @export
assert_has_valid_names <- assertHasValidNames  # nolint



#' @rdname checkHasValidNames
#' @export
expect_has_valid_names <- makeExpectationFunction(checkHasValidNames)  # nolint
