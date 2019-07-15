#' Does the input contain a ggplot2 scale?
#'
#' @export
#' @inherit params
#'
#' @param scale `character(1)`.
#'   Type of scale, either `"continuous"` or `"discrete"`.
#' @param aes `character(1)`.
#'   Aesthetic mapping, either `"colour"` or `"fill"`. Note that ggplot2 prefers
#'   British spelling, so we're enforcing that convention here.
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
#' isGGScale(x = colour_c, scale = "continuous", aes = "colour")
#' isGGScale(x = colour_d, scale = "discrete", aes = "colour")
#' isGGScale(x = fill_c, scale = "continuous", aes = "fill")
#' isGGScale(x = fill_d, scale = "discrete", aes = "fill")

# Updated 2019-07-15.
isGGScale <- function(
    x,
    scale = c("continuous", "discrete"),
    aes = c("colour", "fill"),
    nullOK = FALSE
) {
    scale <- match.arg(scale)
    aes <- match.arg(aes)
    assert(isFlag(nullOK))

    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    # Check that the object inherits all of the required classes.
    ok <- isAll(
        x = x,
        classes = c(
            paste0("Scale", .capitalize(scale)),
            "Scale",
            "ggproto",
            "gg"
        )
    )
    if (!isTRUE(ok)) return(ok)

    # Note that this has to match the British spelling (e.g colour).
    ok <- identical(x = x[["aesthetics"]], y = aes)
    if (!isTRUE(ok)) {
        # nocov start
        return(false(
            "%s isn't identical to %. Use British spelling (e.g. colour).",
            x[["aesthetics"]], aes
        ))
        # nocov end
    }

    TRUE
}
