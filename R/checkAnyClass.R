# FIXME `checkClass()` is very strict and doesn't allow for inheritance
# (e.g. atomic, numeric, scalar all fail for integer).
# TODO Consider adding `checkInheritedClass()`?
# TODO Export a variant named `isAny()`?



#' Does an Object Belong to Any of These Classes?
#'
#' @name checkAnyClass
#' @aliases anyClass
#' @inheritParams params
#'
#' @seealso
#' - `checkmate::checkClass()`.
#' - `checkmate::checkMultiClass()`.
#' - `assertive::is2()`.
#' - `assertive::assert_is_any_of()`.
#'
#' @examples
#' # Define an integer.
#' x <- 1L
#'
#' testClass(x, classes = c("integer", "numeric", "character", "atomic"))
#'
#' # This should pass, but fails...very strict.
#' checkClass(x, "numeric")
#' checkClass(x, "character")
#'
#'
NULL



.anyClass <- function(x, classes) {
    any(vapply(
        X = classes,
        x = x,
        FUN = function(class, x) {
            testClass(x = x, classes = class)
        },
        FUN.VALUE = logical(1L)
    ))
}



# TODO Improve the error message here.
#' @rdname checkAnyClass
#' @export
checkAnyClass <- function(x, classes) {
    if (isTRUE(.anyClass(x, classes))) {
        TRUE
    } else {
        "Object does not belong to any of these classes"
    }
}



#' @rdname checkAnyClass
#' @export
check_any_class <- checkAnyClass



#' @rdname checkAnyClass
#' @export
testAnyClass <- makeTestFunction(checkAnyClass)



#' @rdname checkAnyClass
#' @export
test_any_class <- testAnyClass



#' @rdname checkAnyClass
#' @export
assertAnyClass <- makeAssertionFunction(checkAnyClass)



#' @rdname checkAnyClass
#' @export
assert_any_class <- assertAnyClass



#' @rdname checkAnyClass
#' @export
expect_any_class <- makeExpectationFunction(checkAnyClass)
