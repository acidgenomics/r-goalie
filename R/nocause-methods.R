#' Return standard logical without cause attribute
#'
#' @name nocause
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
#' class(x)
#' xx <- nocause(x)
#' print(xx)
#' class(xx)
NULL



## Updated 2021-02-23.
`nocause,goalie` <-  # nolint
    function(object) {
        names <- names(object)
        object <- as.logical(object)
        names(object) <- names
        object
    }



#' @rdname nocause
#' @export
setMethod(
    f = "nocause",
    signature = signature("goalie"),
    definition = `nocause,goalie`
)
