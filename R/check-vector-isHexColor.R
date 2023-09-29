#' Does the input contain hexadecimal colors?
#'
#' @name check-vector-isHexColor
#' @note Updated 2020-04-07.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `assertive::is_hex_color()`.
#'
#' @examples
#' ## TRUE ====
#' x <- viridis::viridis(n = 2L)
#' class(x)
#' print(x)
#' isHexColor(x)
#'
#' ## FALSE ====
#' x <- ggplot2::scale_color_manual
#' class(x)
#' isHexColor(x)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isHexColor Vectorized.
#' @export
isHexColor <- function(x) {
    ## FIXME Need to improve vectorization here.
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ## NOTE `viridis()` adds an extra "FF" to the end of hex color return.
    pattern <- "^(#[0-9A-F]{6})"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    setCause(ok, false = sprintf("doesn't match {.var %s}", pattern))
}



## Scalar ======================================================================

#' @describeIn check-vector-isHexColor Scalar.
#' @export
allAreHexColors <- function(x) {
    ok <- isHexColor(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
