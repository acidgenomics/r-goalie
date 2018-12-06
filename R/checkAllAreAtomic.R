#' Does an Object Contain Elements That Are All Atomic?
#'
#' @name checkAllAreAtomic
#' @aliases allAreAtomic
#' @inherit params
#'
#' @examples
#' checkAllAreAtomic(datasets::mtcars)
NULL



.allAreAtomic <- function(x) {
    all(vapply(
        X = x,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    ))
}



#' @rdname checkAllAreAtomic
#' @export
checkAllAreAtomic <- function(x) {
    if (isTRUE(.allAreAtomic(x))) {
        TRUE
    } else {
        "Not all elements in the object are atomic"
    }
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
