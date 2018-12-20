# FIXME deprecate `contains*`.



#' Does the input contain hexadecimal colors?
#'
#' @name isHexColor
#' @inherit params
#' @export
#'
#' @seealso `assertive::is_hex_color()`.
#'
#' @examples
#' ## Pass ====
#' x <- viridis::viridis(n = 2L)
#' class(x)
#' print(x)
#' isHexColor(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' class(x)
#' isHexColor(x)
isHexColor <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    # NOTE `viridis()` adds an extra "FF" to the end of hex color return.
    pattern <- "^(#[0-9A-F]{6})"
    ok <- isMatchingRegex(x = x, pattern = pattern, .xname = .xname)
    if (!all(ok)) return(ok)

    TRUE
}



#' @rdname isHexColor
#' @export
allAreHexColors <- function(x, .xname = getNameInParent(x)) {
    ok <- isHexColor(x = x, .xname = .xname)
    if (!all(ok)) return(ok)
    TRUE
}
