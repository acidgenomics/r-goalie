#' Does an Object Contain Elements That Are All Atomic?
#'
#' @aliases allAreAtomic all_are_atomic
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



#' @describeIn checkAllAreAtomic snake alias.
#' @export
check_all_are_atomic <-  # nolint
    checkAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
testAllAreAtomic <- makeTestFunction(checkAllAreAtomic)



#' @describeIn checkAllAreAtomic snake alias.
#' @export
test_all_are_atomic <-  # nolint
    testAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
assertAllAreAtomic <- makeAssertionFunction(checkAllAreAtomic)



#' @describeIn checkAllAreAtomic snake alias.
#' @export
assert_all_are_atomic <-  # nolint
    assertAllAreAtomic



#' @rdname checkAllAreAtomic
#' @export
expect_all_are_atomic <-  # nolint
    makeExpectationFunction(checkAllAreAtomic)
