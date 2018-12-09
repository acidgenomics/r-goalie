#' Does an Object Contain Any Information?
#'
#' @aliases hasLength has_length
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' checkHasLength(c("foo", "bar"))
#' checkHasLength(matrix(data = seq_len(100L), nrow = 10L))
#'
#' ## Fail ====
#' checkHasLength(NULL)
#' checkHasLength(character())
#' checkHasLength(matrix())
checkHasLength <- function(x) {
    ok <- length(x) > 0L
    if (!isTRUE(ok)) {
        return("Must have length, otherwise considered empty")
    }
    TRUE
}



#' @rdname checkHasLength
#' @export
check_has_length <- checkHasLength  # nolint



#' @rdname checkHasLength
#' @export
testHasLength <- makeTestFunction(checkHasLength)



#' @rdname checkHasLength
#' @export
test_has_length <- testHasLength  # nolint



#' @rdname checkHasLength
#' @export
assertHasLength <- makeAssertionFunction(checkHasLength)



#' @rdname checkHasLength
#' @export
assert_has_length <- assertHasLength  # nolint



#' @rdname checkHasLength
#' @export
expect_has_length <- makeExpectationFunction(checkHasLength)  # nolint
