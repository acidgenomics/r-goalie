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



#' @describeIn checkIdentical snake alias.
#' @export
check_identical <-  # nolint
    checkIdentical



#' @rdname checkIdentical
#' @export
testIdentical <- makeTestFunction(checkIdentical)



#' @describeIn checkIdentical snake alias.
#' @export
test_identical <-  # nolint
    testIdentical



#' @rdname checkIdentical
#' @export
assertIdentical <- makeAssertionFunction(checkIdentical)



#' @describeIn checkIdentical snake alias.
#' @export
assert_identical <-  # nolint
    assertIdentical



# expect_identical is already exported by testthat.
