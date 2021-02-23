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
#' x <- goalie(object = c(FALSE, TRUE), cause = "Element 1 is FALSE.")
#' x <- cause(x)
#' print(x)
NULL



## Updated 2021-02-23.
`cause,goalie` <-  # nolint
    function(object) {
        slot(object, name = "cause")
    }



#' @rdname cause
#' @export
setMethod(
    f = "cause",
    signature = signature("goalie"),
    definition = `cause,goalie`
)
