#' Does the input contain hexadecimal colors?
#'
#' @name containsHexColors
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
#' containsHexColors(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' class(x)
#' containsHexColors(x)
containsHexColors <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }

    # TODO Switch to `isMatchingRegex()` here.
    # NOTE `viridis()` adds an extra "FF" to the end of hex color return.
    pattern <- "^(#[0-9A-F]{6})"
    ok <- all(grepl(pattern = pattern, x = x, ignore.case = TRUE))
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "%s does not contain hexadecimal colors.\n",
                "For example, use #FF0000 to indicate red."
            ),
            .xname
        ))
    }

    TRUE
}



# Soft deprecate?
#' @rdname containsHexColors
#' @export
areHexColors <- containsHexColors
