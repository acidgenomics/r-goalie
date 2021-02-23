## FIXME WHEN ANY ELEMENTS ARE FALSE, CAUSE NEEDS TO MATCH THE INPUT...
## FIXME THIS WILL IMPROVE VECTORIZED SUPPORT.
## FIXME TRUE ELEMENTS SHOULD RETURN NA_CHARACTER_ HERE...



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
        if (isTRUE(all(object))) {
            if (!identical(cause, character())) {
                return("'cause' attribute is not allowed for TRUE return.")
            }
        } else {
            if (!identical(length(object), length(cause))) {
                return("Cause attribute not the same length as check return.")
            }
            ## FIXME DONT ALLOW NAMES IN THE CAUSE ATTRIBUTE...
            ## FIXME CHECK FOR NA VALUES IF ANY ARE TRUE...
        }
        TRUE
    }
)
