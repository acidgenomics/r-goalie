#' Does the Argument Contain a ggplot2 Scale?
#'
#' @name isScale
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
#' isScale(x = colour_c, scale = "continuous", aes = "colour")
#' isScale(x = colour_d, scale = "discrete", aes = "colour")
#' isScale(x = fill_c, scale = "continuous", aes = "fill")
#' isScale(x = fill_d, scale = "discrete", aes = "fill")
NULL



.isScale <- function(
    x,
    scale = c("continuous", "discrete"),
    aes = c("colour", "fill"),
    null.ok = FALSE  # nolint
) {
    scale <- match.arg(scale)
    aes <- match.arg(aes)

    # Allow NULL input, if desired. This is useful for plotting functions
    # where we don't want the user to have to define manually.
    assert(isFlag(null.ok))
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }

    classes <- c(
        paste0("Scale", capitalize(scale)),
        "Scale",
        "ggproto",
        "gg"
    )
    ok <- isAll(x = x, classes = classes)
    if (!isTRUE(ok)) {
        return(FALSE)
    }

    # Note that this has to match the British spelling, if necessary.
    ok <- identical(x = x[["aesthetics"]], y = aes)
    if (!isTRUE(ok)) {
        return(FALSE)
    }

    TRUE
}


#' @rdname checkScale
#' @export
isScale <- makeTestFunction(.isScale)
