#' Does the input contain a (non-empty) character string?
#'
#' @export
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' isString("hello")
#'
#' ## Fail ====
#' isString(1)
#' isString("")
#' isString(NA_character_)
isString <- function(x) {
    xname <- getNameInParent(x)

    ok <- is.character(x) && length(x) == 1L
    if (!isTRUE(ok)) {
        return(false("%s is not a character of length 1.", xname))
    }

    # Return FALSE on NA character or empty string.
    if (is.na(x)) {
        return(false("%s is NA.", xname))
    }
    if (identical(x, "")) {
        return(false("%s contains empty string.", xname))
    }

    TRUE
}
