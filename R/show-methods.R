#' Show an object
#'
#' @name show
#' @author Michael Steinbaugh
#' @note Updated 2023-10-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' ## Match the default logical print method.
#' x <- goalie(
#'     object = c(FALSE, TRUE),
#'     cause = c("xxx", NA_character_)
#' )
#' show(x)
NULL



## Updated 2023-10-02.
`show,goalie` <- # nolint
    function(object) {
        x <- as.logical(object)
        x <- unname(x)
        show(x)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "goalie"),
    definition = `show,goalie`
)
