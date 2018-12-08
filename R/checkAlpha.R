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



#' @rdname checkAlpha
#' @export
check_alpha <- checkAlpha  # nolint



#' @rdname checkAlpha
#' @export
testAlpha <- makeTestFunction(checkAlpha)



#' @rdname checkAlpha
#' @export
test_alpha <- testAlpha  # nolint



#' @rdname checkAlpha
#' @export
assertAlpha <- makeAssertionFunction(checkAlpha)



#' @rdname checkAlpha
#' @export
assert_alpha <- assertAlpha  # nolint



#' @rdname checkAlpha
#' @export
expect_alpha <- makeExpectationFunction(checkAlpha)  # nolint
