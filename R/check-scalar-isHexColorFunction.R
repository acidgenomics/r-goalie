#' Does the argument contain a function that returns hexadecimal colors?
#'
#' This assert check is intended primarily to check for RColorBrewer or viridis
#' hexadecimal color value return.
#'
#' @name check-scalar-isHexColorFunction
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso RColorBrewer, viridis packages.
#'
#' @examples
#' ## TRUE ====
#' x <- viridis::viridis
#' isHexColorFunction(x)
#'
#' ## FALSE ====
#' x <- ggplot2::scale_colour_manual
#' isHexColorFunction(x)
NULL



#' @rdname check-scalar-isHexColorFunction
#' @export
isHexColorFunction <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ## Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    ## Check for function.
    ok <- is.function(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not a function.", .xname))
    }
    ## Check for `n` formal.
    ok <- "n" %in% formalArgs(x)
    if (!isTRUE(ok)) {
        return(false("'%s' does not contain an 'n' argument.", .xname))
    }
    ## Check for hex value return.
    colors <- x(n = 2L)
    if (!is.character(colors) || length(colors) == 0L) {
        return(false("'%s' function didn't return any hex colors.", .xname))
    }
    ok <- allAreHexColors(colors)
    if (!isTRUE(ok)) return(ok)
    TRUE
}
