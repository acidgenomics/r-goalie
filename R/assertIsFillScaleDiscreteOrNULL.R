#' Assert Is Fill Palette Scale Discrete or NULL
#'
#' @inherit assert
#'
#' @export
#'
#' @examples
#' fill <- ggplot2::scale_fill_manual(values = c("red", "blue"))
#' class(fill)
#' assertIsFillScaleDiscreteOrNULL(fill)
#' assertIsFillScaleDiscreteOrNULL(NULL)
assertIsFillScaleDiscreteOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("ScaleDiscrete", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleDiscrete")
        )
        assert_are_identical(object[["aesthetics"]], "fill")
    }
}
