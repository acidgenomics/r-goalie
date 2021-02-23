#' @name show
#' @author Michael Steinbaugh
#' @inherit AcidGenerics::show
#' @note Updated 2021-02-23.
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
    signature = signature("goalie"),
    definition = `show,goalie`
)
