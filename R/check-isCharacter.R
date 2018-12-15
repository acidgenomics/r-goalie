#' Does the Input Contain a (Non-Empty) Character Vector?
#'
#' @section Enforced parameters:
#'
#' - Must have length.
#' - Cannot contain `NA` strings.
#' - Cannot contain empty (`""`) strings.
#'
#' @name isCharacter
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' isCharacter("a")
#'
#' ## Fail ====
#' isCharacter(NULL)
#' isCharacter(character())
#' isCharacter("")
#' isCharacter(NA_character_)
NULL



#' @rdname isCharacter
#' @importFrom assertive.strings is_non_missing_nor_empty_character
#' @export
isCharacter <- function(x) {
    if (!is(x, "character")) {
        return(FALSE)
    }
    if (length(x) == 0L) {
        return(FALSE)
    }
    all(is_non_missing_nor_empty_character(x))
}
