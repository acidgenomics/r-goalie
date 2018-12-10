#' Does the Argument Contain a Function That Returns Hexadecimal Colors?
#'
#' This assert check is intended primarily to check for RColorBrewer or viridis
#' hexadecimal color value return.
#'
#' @name isHexColorFunction
#' @inherit params
#'
#' @seealso RColorBrewer, viridis packages.
#'
#' @examples
#' ## Pass ====
#' x <- viridis::viridis
#' isHexColorFunction(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' isHexColorFunction(x)
NULL



.isHexColorFunction <- function(x) {
    if (!is.function(x)) {
        return("Must contain a function")
    }

    # Check for `n` formal.
    if (!"n" %in% formalArgs(x)) {
        return("Hex color function must contain `n` formal")
    }

    colors <- x(n = 2L)
    if (
        !is.character(colors) ||
        length(colors) == 0L
    ) {
        return("Hex color function didn't return any values")
    }

    check <- .containsHexColors(colors)
    if (is.character(check)) {
        return(check)
    }

    TRUE
}



#' @rdname isHexColorFunction
#' @export
isHexColorFunction <- makeTestFunction(.isHexColorFunction)
