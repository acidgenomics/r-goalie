#' Check If an Argument Contains an Alpha Level
#'
#' An alpha level must be a `scalar numeric` greater than 0 and less than 1.
#'
#' @name alpha
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



#' @rdname alpha
#' @export
checkAlpha <- function(x) {
    if (isTRUE(.alpha(x))) {
        TRUE
    } else {
        "An alpha level must be a scalar numeric > 0 and < 1"
    }
}



#' @rdname alpha
#' @export
check_alpha <- checkAlpha



#' @rdname alpha
#' @export
testAlpha <- makeTestFunction(checkAlpha)



#' @rdname alpha
#' @export
test_alpha <- testAlpha



#' @rdname alpha
#' @export
assertAlpha <- makeAssertionFunction(checkAlpha)



#' @rdname alpha
#' @export
assert_alpha <- assertAlpha



#' @rdname alpha
#' @export
expect_alpha <- makeExpectationFunction(checkAlpha)
