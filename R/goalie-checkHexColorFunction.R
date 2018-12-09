#' Does the Argument Contain a Function That Returns Hexadecimal Colors?
#'
#' This assert check is intended primarily to check for RColorBrewer or viridis
#' hexadecimal color value return.
#'
#' @aliases hexColorFunction hex_color_function
#' @inherit params
#' @export
#'
#' @seealso RColorBrewer, viridis
#'
#' @examples
#' ## Pass ====
#' x <- viridis::viridis
#' checkHexColorFunction(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' checkHexColorFunction(x)
checkHexColorFunction <- function(
    x,
    null.ok = FALSE  # nolint
) {
    # Allow NULL input, if desired. This is useful for plotting functions where
    # we don't want the user to have to define manually.
    assertFlag(null.ok)
    if (is.null(x) && null.ok) {
        return(TRUE)
    }

    if (!is.function(x)) {
        return("Must contain a function")
    }

    # Check for `n` formal.
    if (!"n" %in% formalArgs(x)) {
        return("Hex color function must contain `n` formal")
    }

    colors <- x(n = 2L)
    if (
        !is.character(colors) ||
        length(colors) == 0L
    ) {
        return("Hex color function didn't return any values")
    }

    check <- checkHexColor(colors)
    if (is.character(check)) {
        return(check)
    }

    TRUE
}



#' @rdname checkHexColorFunction
#' @export
check_hex_color_function <- checkHexColorFunction  # nolint



#' @rdname checkHexColorFunction
#' @export
testHexColorFunction <- makeTestFunction(checkHexColorFunction)



#' @rdname checkHexColorFunction
#' @export
test_hex_color_function <- checkHexColorFunction  # nolint



#' @rdname checkHexColorFunction
#' @export
assertHexColorFunction <- makeAssertionFunction(checkHexColorFunction)



#' @rdname checkHexColorFunction
#' @export
assert_hex_color_function <- assertHexColorFunction  # nolint



#' @rdname checkHexColorFunction
#' @export
expect_hex_color_function <-  # nolint
    makeExpectationFunction(checkHexColorFunction)
