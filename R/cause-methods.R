#' Cause attribute
#'
#' @name cause
#' @note Updated 2023-10-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#'
#' @examples
#' x <- goalie(
#'     object = c(FALSE, TRUE),
#'     cause = c("xxx", NA_character_)
#' )
#' print(x)
#' x <- cause(x)
#' print(x)
NULL



## Updated 2023-09-29.
`cause,goalie` <- # nolint
    function(object) {
        slot(object, name = "cause")
    }



#' @rdname cause
#' @export
setMethod(
    f = "cause",
    signature = signature(object = "goalie"),
    definition = `cause,goalie`
)
