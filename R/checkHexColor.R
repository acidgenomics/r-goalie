#' Does the Argument Contain Hexadecimal Colors?
#'
#' @aliases hexColor hex_color
#' @inherit params
#' @export
#'
#' @seealso `assertive::is_hex_color()`.
#'
#' @examples
#' ## Pass ====
#' x <- viridis::viridis(n = 2L)
#' checkHexColorFunction(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' checkHexColorFunction(x)
checkHexColor <- function(x) {
    if (!is.character(x)) {
        return("Must contain character")
    }

    # NOTE `viridis()` adds an extra "FF" to the end of hex color return.
    pattern <- "^(#[0-9A-F]{6})"
    ok <- all(grepl(
        pattern = pattern,
        x = x,
        ignore.case = TRUE
    ))
    if (!ok) {
        return(paste0(
            "Must contain hexadecimal colors.\n",
            "For example, use #FF0000 to indicate red."
        ))
    }

    TRUE
}



#' @rdname checkHexColor
#' @export
check_hex_color <- checkHexColor  # nolint



#' @rdname checkHexColor
#' @export
testHexColor <- makeTestFunction(checkHexColor)



#' @rdname checkHexColor
#' @export
test_hex_color <- testHexColor  # nolint



#' @rdname checkHexColor
#' @export
assertHexColor <- makeAssertionFunction(checkHexColor)



#' @rdname checkHexColor
#' @export
assert_hex_color <- assertHexColor  # nolint



#' @rdname checkHexColor
#' @export
expect_hex_color <- makeExpectationFunction(checkHexColor)  # nolint
