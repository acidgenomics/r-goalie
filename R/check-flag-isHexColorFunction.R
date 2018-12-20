#' Does the argument contain a function that returns hexadecimal colors?
#'
#' This assert check is intended primarily to check for RColorBrewer or viridis
#' hexadecimal color value return.
#'
#' @export
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
isHexColorFunction <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    # Check for function.
    ok <- is.function(x)
    if (!isTRUE(ok)) {
        return(false("%s is not a function.", .xname))
    }

    # Check for `n` formal.
    ok <- "n" %in% formalArgs(x)
    if (!isTRUE(ok)) {
        return(false("Hex color function must contain `n` formal."))
    }

    colors <- x(n = 2L)
    if (!is.character(colors) || length(colors) == 0L) {
        return(false("Hex color function didn't return any values."))
    }

    ok <- containsHexColors(colors)
    if (!isTRUE(ok)) return(ok)

    TRUE
}
