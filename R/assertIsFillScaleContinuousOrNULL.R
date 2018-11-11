#' Assert Is Fill Palette Scale Continuous or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' fill <- ggplot2::scale_fill_gradient(low = "red", high = "blue")
#' class(fill)
#' assertIsFillScaleContinuousOrNULL(fill)
#' assertIsFillScaleContinuousOrNULL(NULL)
assertIsFillScaleContinuousOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("ScaleContinuous", "NULL"))
    if (!is.null(object)) {
        assert_is_all_of(
            x = object,
            classes = c("ggproto", "Scale", "ScaleContinuous")
        )
        assert_are_identical(object[["aesthetics"]], "fill")
    }
}
