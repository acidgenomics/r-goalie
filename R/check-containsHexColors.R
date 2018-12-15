#' Does the Input Contain Hexadecimal Colors?
#'
#' @name containsHexColors
#' @inherit params
#' @export
#'
#' @seealso `assertive::is_hex_color`.
#'
#' @examples
#' ## Pass ====
#' x <- viridis::viridis(n = 2L)
#' class(x)
#' containsHexColors(x)
#'
#' ## Fail ====
#' x <- ggplot2::scale_colour_manual
#' class(x)
#' containsHexColors(x)
NULL



.containsHexColors <- function(x) {
    if (!is.character(x)) {
        return("Must contain character")
    }

    # NOTE `viridis` adds an extra "FF" to the end of hex color return.
    pattern <- "^(#[0-9A-F]{6})"
    ok <- all(grepl(
        pattern = pattern,
        x = x,
        ignore.case = TRUE
    ))
    if (!ok) {
        return(paste0(
            "Must contain hexadecimal colors.\n",
            "For example, use #FF0000 to indicate red."
        ))
    }

    TRUE
}



#' @rdname containsHexColors
#' @export
containsHexColors <- makeTestFunction(.containsHexColors)
