#' Does an Object Contain Any Information?
#'
#' @aliases nonEmpty non_empty
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' checkNonEmpty(c("foo", "bar"))
#' checkNonEmpty(matrix(data = seq_len(100L), nrow = 10L))
#'
#' ## Fail ====
#' checkNonEmpty(NULL)
#' checkNonEmpty(character())
#' checkNonEmpty(matrix())
checkNonEmpty <- function(x) {
    ok <- length(x) > 0L
    if (!isTRUE(ok)) {
        return("Must have length, otherwise considered empty")
    }
    TRUE
}



#' @rdname checkNonEmpty
#' @export
check_non_empty <- checkNonEmpty  # nolint



#' @rdname checkNonEmpty
#' @export
testNonEmpty <- makeTestFunction(checkNonEmpty)



#' @rdname checkNonEmpty
#' @export
test_non_empty <- testNonEmpty  # nolint



#' @rdname checkNonEmpty
#' @export
assertNonEmpty <- makeAssertionFunction(checkNonEmpty)



#' @rdname checkNonEmpty
#' @export
assert_non_empty <- assertNonEmpty  # nolint



#' @rdname checkNonEmpty
#' @export
expect_non_empty <- makeExpectationFunction(checkNonEmpty)  # nolint
