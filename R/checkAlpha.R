#' Check If an Argument Contains an Alpha Level
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



.alpha <- function(x) {
    if (!is_scalar_double(x)) {
        return(FALSE)
    }
    x > 0L && x < 1L
}



#' @rdname checkAlpha
#' @export
checkAlpha <- function(x) {
    if (isTRUE(.alpha(x))) {
        TRUE
    } else {
        "An alpha level must be a scalar numeric > 0 and < 1"
    }
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
