## FIXME cause slot should only allow character(1) or NULL.



#' goalie check
#'
#' @details
#' Contains a `logical` with `cause` if any elements are FALSE.
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
        TRUE
    }
)
