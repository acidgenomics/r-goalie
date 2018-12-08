#' Does an Object Contain Elements That Are All Atomic?
#'
#' @aliases allAreAtomic
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' checkAllAreAtomic(data.frame(a = "foo", b = "bar"))
#' checkAllAreAtomic(list(a = "foo", b = "bar"))
#'
#' ## Fail ====
#' checkAllAreAtomic(list(a = "x", b = list()))
checkAllAreAtomic <- function(x) {
    ok <- all(vapply(
        X = x,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    ))
    if (!ok) {
        return("Not all elements in the object are atomic")
    }
    TRUE
}



#' @rdname checkAllAreAtomic
#' @export
check_all_are_atomic <- checkAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
testAllAreAtomic <- makeTestFunction(checkAllAreAtomic)



#' @rdname checkAllAreAtomic
#' @export
test_all_are_atomic <- testAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
assertAllAreAtomic <- makeAssertionFunction(checkAllAreAtomic)



#' @rdname checkAllAreAtomic
#' @export
assert_all_are_atomic <- assertAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
expect_all_are_atomic <- makeExpectationFunction(checkAllAreAtomic)
