#' Show an object
#'
#' @name show
#' @author Michael Steinbaugh
#' @note Updated 2022-02-07.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' ## Match the default logical print method.
#' x <- goalie(
#'     object = c("aaa" = FALSE, "bbb" = TRUE),
#'     cause = c(
#'         "'aaa' is FALSE.",
#'         NA_character_
#'     )
#' )
#' show(x)
NULL



## Updated 2021-02-23.
`show,goalie` <-  # nolint
    function(object) {
        x <- as.logical(object)
        names(x) <- names(object)
        show(x)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "goalie"),
    definition = `show,goalie`
)
