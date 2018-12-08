# FIXME Switch to using `null.ok` approach like `checkScale()`.



#' Does the Argument Contain a Hex Color Function?
#'
#' @aliases hexColorFunction hex_color_function
#' @inherit params
#' @export
#'
#' @examples
#' x <- function(n) {
#'     colors <- c("#FFFFFF", "#000000")
#'     colors[seq_len(n)]
#' }
#' checkHexColorFunction(x)
checkHexColorFunction <- function(x, null.ok = FALSE) {
    # Allow NULL input, if desired. This is useful for plotting functions where
    # we don't want the user to have to define manually.
    assertFlag(null.ok)
    if (is.null(x) && null.ok) {
        return(TRUE)
    }

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

    # viridis adds "FF" to the end of hex colors.
    # Attempt to fix before running hex check.
    colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)

    if (!all(is_hex_color(colors))) {
        return("Function doesn't appear to return hex colors")
    }

    TRUE
}
