#' goalie check
#'
#' @details
#' Contains a `logical` with `cause` of `character(1)` if any elements are
#' `FALSE`.
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @return `goalie`.
setClass(
    Class = "goalie",
    contains = "logical",
    slots = list(
        "cause" = "character"
    )
)
setValidity(
    Class = "goalie",
    method = function(object) {
        if (!isTRUE(length(object) > 0)) {
            return("Object is 'logical(0)'.")
        }
        cause <- slot(object, name = "cause")
        if (!isTRUE(is.character(cause))) {
            return("Cause attribute is not character.")
        }
        if (!identical(length(object), length(cause))) {
            return("Cause attribute not the same length as check return.")
        }
        if (!is.null(names(cause))) {
            return("Cause attribute has names assigned.")
        }

        if (isTRUE(all(object))) {
            ok <- vapply(
                X = cause,
                FUN = identical,
                y = NA_character_,
                FUN.VALUE = logical(1),
                USE.NAMES = FALSE
            )
            if (!isTRUE(all(ok))) {
                return("TRUE values must have 'NA_character_' cause.")
            }
        }
        if (isTRUE(any(object))) {
            ## FIXME ONLY ALLOW NAS HERE.
        } else {
            ## FIXME DONT ALLOW ANY NA OR EMPTY STRINGS HERE.
        }
        TRUE
    }
)
