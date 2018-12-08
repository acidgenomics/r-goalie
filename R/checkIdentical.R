#' Are Two Objects Identical?
#'
#' @aliases identical
#' @inherit params
#' @export
#'
#' @seealso `identical()`.
#'
#' @examples
#' ## Pass ====
#' checkIdentical(x = 1L, y = 1L)
#'
#' ## Fail ====
#' checkIdentical(x = 1L, y = 1)
checkIdentical <- function(x, y) {
    ok <- identical(x, y)
    if (!ok) {
        return("Objects are not identical")
    }
}



#' @rdname checkIdentical
#' @export
check_identical <- checkIdentical  # nolint



#' @rdname checkIdentical
#' @export
testIdentical <- makeTestFunction(checkIdentical)



#' @rdname checkIdentical
#' @export
test_identical <- testIdentical  # nolint



#' @rdname checkIdentical
#' @export
assertIdentical <- makeAssertionFunction(checkIdentical)



#' @rdname checkIdentical
#' @export
assert_identical <- assertIdentical  # nolint



# expect_identical is already exported by testthat.
