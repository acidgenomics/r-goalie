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
        if (isTRUE(all(object))) {
            if (!identical(cause, character())) {
                return("'cause' attribute is not allowed for TRUE return.")
            }
        } else {
            if (!isTRUE(is.character(cause) && length(cause) == 1L)) {
                return("'cause' attribute is not 'character(1)'.")
            }
        }
        TRUE
    }
)
