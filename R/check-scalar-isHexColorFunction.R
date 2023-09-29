#' Does the argument contain a function that returns hexadecimal colors?
#'
#' This assert check is intended primarily to check for RColorBrewer or viridis
#' hexadecimal color value return.
#'
#' @name check-scalar-isHexColorFunction
#' @note Updated 2022-10-18.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso RColorBrewer, viridis packages.
#'
#' @examples
#' ## TRUE ====
#' x <- viridis::viridis
#' isHexColorFunction(x)
#'
#' ## FALSE ====
#' x <- ggplot2::scale_color_manual
#' isHexColorFunction(x)
NULL



#' @rdname check-scalar-isHexColorFunction
#' @export
isHexColorFunction <-
    function(x, nullOk = FALSE) {
        if (isTRUE(nullOk) && is.null(x)) {
            return(TRUE)
        }
        ## Check for function.
        ok <- is.function(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not a function.",
                toCauseName(x)
            ))
        }
        ## Check for `n` formal.
        ok <- isSubset("n", formalArgs(x))
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't contain an {.arg %s} argument.",
                toCauseName(x), "n"
            ))
        }
        ## Check for hex value return.
        colors <- x(n = 2L)
        if (!is.character(colors) || identical(length(colors), 0L)) {
            return(false(
                "{.var %s} function didn't return any hex colors.",
                toCauseName(x)
            ))
        }
        ok <- allAreHexColors(colors)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
