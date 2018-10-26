#' Assert Is Color Palette Scale Discrete or NULL
#'
#' @inherit assert
#' @export
#'
#' @examples
#' color <- ggplot2::scale_color_manual(values = c("red", "blue"))
#' class(color)
#' assertIsColorScaleDiscreteOrNULL(color)
#' assertIsColorScaleDiscreteOrNULL(NULL)
assertIsColorScaleDiscreteOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        # Note that this has to match the British spelling.
        assert_are_identical(object[["aesthetics"]], "colour")
    }
}
