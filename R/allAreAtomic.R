#' Does an Object Contain Elements That Are All Atomic?
#'
#' @name allAreAtomic
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



#' @rdname allAreAtomic
#' @export
checkAllAreAtomic <- function(x) {
    if (isTRUE(.allAreAtomic(x))) {
        TRUE
    } else {
        "Not all elements in the object are atomic"
    }
}



#' @rdname allAreAtomic
#' @export
check_all_are_atomic <- checkAllAreAtomic



#' @rdname allAreAtomic
#' @export
testAllAreAtomic <- makeTestFunction(checkAllAreAtomic)



#' @rdname allAreAtomic
#' @export
test_all_are_atomic <- testAllAreAtomic



#' @rdname allAreAtomic
#' @export
assertAllAreAtomic <- makeAssertionFunction(checkAllAreAtomic)



#' @rdname allAreAtomic
#' @export
assert_all_are_atomic <- assertAllAreAtomic



#' @rdname allAreAtomic
#' @export
expect_all_are_atomic <- makeExpectationFunction(checkAllAreAtomic)
