#' goalie logical assert check return
#'
#' Contains a `logical` with `cause` attributes.
#'
#' @export
#' @note Updated 2023-10-02.
#'
#' @return `goalie`.
setClass(
    Class = "goalie",
    contains = "logical",
    slots = list("cause" = "character")
)
setValidity(
    Class = "goalie",
    method = function(object) {
        cause <- slot(object, name = "cause")
        if (anyNA(object)) {
            return("Object contains NA.")
        }
        if (!is.null(names(object))) {
            return("Object has names assigned.")
        }
        if (!is.character(cause)) {
            return("Cause attribute is not character.")
        }
        if (!identical(length(object), length(cause))) {
            return("Cause attribute not the same length as check return.")
        }
        if (length(object) > 1L && is.null(names(cause))) {
            return("Cause attribute doesn't have names assigned.")
        }
        if (any(object)) {
            ok <- vapply(
                X = cause[which(object == TRUE)],
                FUN = identical,
                y = NA_character_,
                FUN.VALUE = logical(1L),
                USE.NAMES = FALSE
            )
            if (!all(ok)) {
                return("TRUE values must have NA cause.")
            }
        }
        ok <- vapply(
            X = cause[which(object == FALSE)],
            FUN = function(x) {
                isTRUE(nzchar(x)) || return(FALSE)
                isFALSE(is.na(x)) || return(FALSE)
                TRUE
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = FALSE
        )
        if (!all(ok)) {
            return("FALSE values must have non-empty character cause.")
        }
        TRUE
    }
)
