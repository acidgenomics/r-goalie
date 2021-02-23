#' @name show
#' @author Michael Steinbaugh
#' @inherit AcidGenerics::show
#' @note Updated 2021-02-23.
#'
#' @examples
#' ## Match the default logical print method.
#' x <- goalie(
#'     x = c("aaa" = FALSE, "bbb" = TRUE),
#'     cause = "'aaa' is FALSE."
#' )
#' show(x)
NULL



## Updated 2021-02-23.
`show,goalie` <- function(object) {
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
