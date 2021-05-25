#' Cause attribute
#'
#' @name cause
#' @note Updated 2021-02-23.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character`.
#'
#' @examples
#' x <- goalie(
#'     object = c(FALSE, TRUE),
#'     cause = c(
#'         "Element 1 is FALSE.",
#'         NA_character_
#'     )
#' )
#' print(x)
#' x <- cause(x)
#' print(x)
NULL



## Updated 2021-02-23.
`cause,goalie` <-  # nolint
    function(object) {
        x <- slot(object, name = "cause")
        names(x) <- names(object)
        x
    }



#' @rdname cause
#' @export
setMethod(
    f = "cause",
    signature = signature("goalie"),
    definition = `cause,goalie`
)
