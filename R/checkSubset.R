#' Does an Argument Match Against a Secondary Argument?
#'
#' @seealso
#' - `match()`, `%in%`.
#' - `checkNames()`.
#'
#' @examples
#' checkSubset(x = "foo", y = c("foo", "bar"))
checkSubset <- function(x, y) {
    ok <- x %in% y
    if (!isTRUE(ok)) {
        return(paste(
            deparse(substitute(x)),
            "is not a subset of",
            deparse(substitute(y))
        ))
    }
    TRUE
}



#' @rdname checkSubset
#' @export
check_subset <- checkSubset



#' @rdname checkSubset
#' @export
testSubset <- makeTestFunction(checkSubset)



#' @rdname checkSubset
#' @export
test_subset <- testSubset



#' @rdname checkSubset
#' @export
assertSubset <- makeAssertionFunction(checkSubset)



#' @rdname checkSubset
#' @export
assert_subset <- assertSubset



#' @rdname checkSubset
#' @export
expect_subset <- makeExpectationFunction(checkSubset)
