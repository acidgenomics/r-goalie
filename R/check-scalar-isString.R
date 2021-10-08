#' Does the input contain a (non-empty) character string?
#'
#' @name check-scalar-isString
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
isString <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.character(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not character.", .xname))
    }
    ok <- !is.na(x)
    ## Return FALSE on NA character or empty string.
    if (!isTRUE(ok)) {
        return(false("{.var %s} is {.val %s}.", .xname, "NA"))
    }
    ok <- !identical(x, "")
    if (!isTRUE(ok)) {
        return(false("{.var %s} contains empty string.", .xname))
    }
    TRUE
}
