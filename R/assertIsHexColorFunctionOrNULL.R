#' Assert Is Hex Color Function or NULL
#'
#' @inherit assert
#' @export
#'
#' @examples
#' hex <- function(n) {
#'     colors <- c("#FFFFFF", "#000000")
#'     colors[seq_len(n)]
#' }
#' assertIsHexColorFunctionOrNULL(hex)
#' assertIsHexColorFunctionOrNULL(NULL)
assertIsHexColorFunctionOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("function", "NULL"))
    if (is.function(object)) {
        colors <- object(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
    }
}
