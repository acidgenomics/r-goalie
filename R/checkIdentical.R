#' Are Two Objects Identical?
#'
#' @aliases allAreAtomic
#' @inherit params
#' @export
#'
#' @seealso `identical()`.
#'
#' @examples
#' checkAllAreAtomic(datasets::mtcars)
checkIdentical <- function(x, y) {
    ok <- identical(x, y)
    if (!ok) {
        return("Objects are not identical")
    }
}



#' @rdname checkIdentical
#' @export
check_identical <- checkIdentical



#' @rdname checkIdentical
#' @export
testIdentical <- makeTestFunction(checkIdentical)



#' @rdname checkIdentical
#' @export
test_identical <- testIdentical



#' @rdname checkIdentical
#' @export
assertIdentical <- makeAssertionFunction(checkIdentical)



#' @rdname checkIdentical
#' @export
assert_identical <- assertIdentical



# expect_identical is already exported by testthat.
