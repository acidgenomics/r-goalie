#' Does the input contain a (non-empty) character string?
#'
#' @name check-scalar-isString
#' @inherit params
#' @note Updated 2019-07-29.
#'
#' @examples
#' ## TRUE ====
#' isString("hello")
#'
#' ## FALSE ====
#' isString(1)
#' isString("")
#' isString(NA_character_)
NULL



#' @rdname check-scalar-isString
#' @export
## Updated 2019-07-15.
isString <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ## Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    ok <- is.character(x) && length(x) == 1L
    if (!isTRUE(ok)) {
        return(false("%s is not a character of length 1.", .xname))
    }

    ## Return FALSE on NA character or empty string.
    if (is.na(x)) {
        return(false("%s is NA.", .xname))
    }
    if (identical(x, "")) {
        return(false("%s contains empty string.", .xname))
    }

    TRUE
}
