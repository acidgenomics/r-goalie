#' Does an Object Belong to Any of These Classes?
#'
#' @name checkAnyClass
#' @aliases anyClass any_class
#' @inheritParams params
#' @export
#'
#' @seealso
#' - `checkmate::checkClass()`.
#' - `checkmate::checkMultiClass()`.
#' - `assertive::is2()`.
#' - `assertive::assert_is_any_of()`.
#'
#' @examples
#' x <- 1L
#'
#' ## Note that `checkAnyClass()` extends `checkClass()`, which  is very strict.
#' is(x, "numeric")
#' checkmate::testClass(x, "numeric")
#'
#' ## Pass ====
#' checkAnyClass(x, classes = c("integer", "character"))
#'
#' ## Fail ====
#' checkAnyClass(x, classes = c("atomic", "numeric"))
checkAnyClass <- function(x, classes) {
    ok <- any(vapply(
        X = classes,
        x = x,
        FUN = function(class, x) {
            testClass(x = x, classes = class)
        },
        FUN.VALUE = logical(1L)
    ))
    if (!ok) {
        # TODO Improve the message here, showing which failed.
        return("Object does not belong to any of these classes")
    }
    TRUE
}



#' @rdname checkAnyClass
#' @export
check_any_class <-  # nolint
    checkAnyClass



#' @rdname checkAnyClass
#' @export
testAnyClass <- makeTestFunction(checkAnyClass)



#' @rdname checkAnyClass
#' @export
test_any_class <-  # nolint
    testAnyClass



#' @rdname checkAnyClass
#' @export
assertAnyClass <- makeAssertionFunction(checkAnyClass)



#' @rdname checkAnyClass
#' @export
assert_any_class <-  # nolint
    assertAnyClass



#' @rdname checkAnyClass
#' @export
expect_any_class <-  # nolint
    makeExpectationFunction(checkAnyClass)
