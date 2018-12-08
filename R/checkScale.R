#' Does the Argument Contain a ggplot2 Scale?
#'
#' @name checkScale
#' @aliases scale
#' @inherit params
#' @export
#'
#' @param scale `string`. Type of scale, either `"continuous"` or `"discrete"`.
#' @param aes `string`. Aesthetic mapping, either `"colour"` or `"fill"`. Note
#'   that ggplot2 prefers British spelling, so we're enforcing that convention
#'   here.
#'
#' @examples
#' library(ggplot2)
#'
#' colour_c <- scale_colour_gradient(low = "red", high = "blue")
#' class(colour_c)
#'
#' colour_d <- scale_colour_manual(values = c("red", "blue"))
#' class(colour_d)
#'
#' fill_c <- scale_fill_gradient(low = "red", high = "blue")
#' class(fill_c)
#'
#' fill_d <- scale_fill_manual(values = c("red", "blue"))
#' class(fill_d)
#'
#' checkScale(x = colour_c, scale = "continuous", aes = "colour")
#' checkScale(x = colour_d, scale = "discrete", aes = "colour")
#' checkScale(x = fill_c, scale = "continuous", aes = "fill")
#' checkScale(x = fill_d, scale = "discrete", aes = "fill")
checkScale <- function(
    x,
    scale = c("continuous", "discrete"),
    aes = c("colour", "fill"),
    null.ok = FALSE  # nolint
) {
    scale <- match.arg(scale)
    aes <- match.arg(aes)

    # Allow NULL input, if desired. This is useful for plotting functions
    # where we don't want the user to have to define manually.
    assertFlag(null.ok)
    if (is.null(x) && null.ok) {
        return(TRUE)
    }

    classes <- c(
        paste0("Scale", capitalize(scale)),
        "Scale",
        "ggproto",
        "gg"
    )
    check <- checkClass(x = x, classes = classes)
    if (is.character(check)) {
        return(check)
    }

    # Note that this has to match the British spelling, if necessary.
    check <- checkIdentical(x = x[["aesthetics"]], y = aes)
    if (is.character(check)) {
        return(check)
    }

    TRUE
}



#' @rdname checkScale
#' @export
check_scale <- checkScale  # nolint



#' @rdname checkScale
#' @export
testScale <- makeTestFunction(checkScale)



#' @rdname checkScale
#' @export
test_scale <- testScale  # nolint



#' @rdname checkScale
#' @export
assertScale <- makeAssertionFunction(checkScale)



#' @rdname checkScale
#' @export
assert_scale <- assertScale  # nolint



#' @rdname checkScale
#' @export
expect_scale <- makeExpectationFunction(checkScale)  # nolint
