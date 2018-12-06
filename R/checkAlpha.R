#' Does the Argument Contain an Alpha Level?
#'
#' An alpha level must be a `scalar numeric` greater than 0 and less than 1.
#'
#' @name checkAlpha
#' @aliases alpha
#' @inherit params
#'
#' @examples
#' checkAlpha(0.05)
NULL



#' @rdname checkAlpha
#' @export
checkAlpha <- function(x) {
    ok <- is_scalar_double(x)
    if (!ok) {
        return("Must be scalar double")
    }
    ok <- x > 0L && x < 1L
    if (!ok) {
        "An alpha level must be a scalar numeric > 0 and < 1"
    }
    TRUE
}



#' @rdname checkAlpha
#' @export
check_alpha <- checkAlpha



#' @rdname checkAlpha
#' @export
testAlpha <- makeTestFunction(checkAlpha)



#' @rdname checkAlpha
#' @export
test_alpha <- testAlpha



#' @rdname checkAlpha
#' @export
assertAlpha <- makeAssertionFunction(checkAlpha)



#' @rdname checkAlpha
#' @export
assert_alpha <- assertAlpha



#' @rdname checkAlpha
#' @export
expect_alpha <- makeExpectationFunction(checkAlpha)
