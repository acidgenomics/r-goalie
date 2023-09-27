#' Does the input contain a ggplot2 scale?
#'
#' @name check-scalar-isGGScale
#' @note Updated 2019-09-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param scale `character(1)`.
#' Type of scale, either `"continuous"` or `"discrete"`.
#'
#' @param aes `character(1)`.
#' Aesthetic mapping, either  `"color"`/`"colour"` or `"fill"`.
#' Note that ggplot2 prefers British spelling.
#'
#' @examples
#' library(ggplot2)
#'
#' color_c <- scale_color_gradient(low = "red", high = "blue")
#' class(color_c)
#'
#' color_d <- scale_color_manual(values = c("red", "blue"))
#' class(color_d)
#'
#' fill_c <- scale_fill_gradient(low = "red", high = "blue")
#' class(fill_c)
#'
#' fill_d <- scale_fill_manual(values = c("red", "blue"))
#' class(fill_d)
#'
#' isGGScale(x = color_c, scale = "continuous", aes = "color")
#' isGGScale(x = color_d, scale = "discrete", aes = "color")
#' isGGScale(x = fill_c, scale = "continuous", aes = "fill")
#' isGGScale(x = fill_d, scale = "discrete", aes = "fill")
NULL



#' @rdname check-scalar-isGGScale
#' @export
## Updated 2019-07-15.
isGGScale <-
    function(x,
             scale = c("continuous", "discrete"),
             aes = c("color", "colour", "fill"),
             nullOk = FALSE) {
        scale <- match.arg(scale)
        aes <- match.arg(aes)
        if (isTRUE(nullOk) && is.null(x)) {
            return(TRUE)
        }
        ## Check that the object inherits all of the required classes.
        ok <- isAll(
            x = x,
            classes = c(
                paste0("Scale", .capitalize(scale)),
                "Scale",
                "ggproto",
                "gg"
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ## Note that this has to match the British spelling (e.g colour).
        if (identical(aes, "color")) {
            aes <- "colour"
        }
        ok <- identical(x = x[["aesthetics"]], y = aes)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
