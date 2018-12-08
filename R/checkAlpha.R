#' Does the Argument Contain an Alpha Level?
#'
#' An alpha level must be a `scalar numeric` greater than 0 and less than 1.
#'
#' @aliases alpha
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' checkAlpha(0.05)
#'
#' ## Fail ====
#' checkAlpha("xxx")
#' checkAlpha(1L)
checkAlpha <- function(x) {
    msg <- "An alpha level must be a scalar numeric > 0 and < 1"
    ok <- is_scalar_double(x)
    if (!ok) {
        return(msg)
    }
    ok <- x > 0L && x < 1L
    if (!ok) {
        return(msg)
    }
    TRUE
}



#' @describeIn checkAlpha snake alias.
#' @export
check_alpha <-  # nolint
    checkAlpha



#' @rdname checkAlpha
#' @export
testAlpha <- makeTestFunction(checkAlpha)



#' @describeIn checkAlpha snake alias.
#' @export
test_alpha <-  # nolint
    testAlpha



#' @rdname checkAlpha
#' @export
assertAlpha <- makeAssertionFunction(checkAlpha)



#' @describeIn checkAlpha snake alias.
#' @export
assert_alpha <-  # nolint
    assertAlpha



#' @rdname checkAlpha
#' @export
expect_alpha <-  # nolint
    makeExpectationFunction(checkAlpha)
