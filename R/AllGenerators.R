## FIXME Enforce only allowing cause when x contains a FALSE.


#' Define a goalie check classed return
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @param object `logical`.
#' @param cause `character(0-1)`.
#'
#' @return `goalie`, which extends `logical`.
#'
#' @examples
#' x <- goalie(
#'     object = c(FALSE, TRUE),
#'     cause = "Element 1 is false."
#' )
#' print(object)
goalie <- function(object, cause = character()) {
    stopifnot(
        is.logical(object),
        is.character(cause)
    )
    new(Class = "goalie", ".Data" = object, "cause" = cause)
}