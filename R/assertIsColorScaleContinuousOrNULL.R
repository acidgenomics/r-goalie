#' Assert Is Color Palette Scale Continuous or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' color <- ggplot2::scale_color_gradient(low = "red", high = "blue")
#' class(color)
#' assertIsColorScaleContinuousOrNULL(color)
#' assertIsColorScaleContinuousOrNULL(NULL)
assertIsColorScaleContinuousOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(object[["aesthetics"]], "colour")
    }
}
