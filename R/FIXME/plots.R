# ggplot2 scale ================================================================
#' Is Scale (or NULL)?
#'
#' @name scale
#' @inherit params
#'
#' @examples
#' color_c <- ggplot2::scale_colour_gradient(low = "red", high = "blue")
#' class(color_c)
#'
#' color_d <- ggplot2::scale_colour_manual(values = c("red", "blue"))
#' class(color_d)
#'
#' fill_c <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
#' class(fill_c)
#'
#' fill_d <- ggplot2::scale_fill_manual(values = c("red", "blue"))
#' class(fill_d)
#'
#' assertIsColorScaleContinuousOrNULL(color_c)
#' assertIsColorScaleDiscreteOrNULL(color_d)
#' assertIsFillScaleContinuousOrNULL(fill_c)
#' assertIsFillScaleDiscreteOrNULL(fill_d)
NULL



#' @rdname scale
#' @export
assertIsColorScaleContinuousOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(x[["aesthetics"]], "colour")
    }
}



#' @rdname scale
#' @export
assertIsColorScaleDiscreteOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(x[["aesthetics"]], "colour")
    }
}



#' @rdname scale
#' @export
assertIsFillScaleContinuousOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        assert_are_identical(x[["aesthetics"]], "fill")
    }
}



#' @rdname scale
#' @export
assertIsFillScaleDiscreteOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(x)) {
        assert_is_all_of(
            x = x,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        assert_are_identical(x[["aesthetics"]], "fill")
    }
}



# Hexadecimal color function ===================================================
#' Is Hex Color Function (or NULL)?
#'
#' @inherit params
#' @export
#'
#' @examples
#' hex <- function(n) {
#'     colors <- c("#FFFFFF", "#000000")
#'     colors[seq_len(n)]
#' }
#' assertIsHexColorFunctionOrNULL(hex)
assertIsHexColorFunctionOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("function", "NULL"))
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
    }
}
