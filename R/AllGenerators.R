## FIXME Enforce only allowing cause when x contains a FALSE.


#' Define a goalie check classed return
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @param x `logical`.
#' @param cause `character(0-1)`.
#'
#' @return `goalie`, which extends `logical`.
#'
#' @examples
#' x <- goalie(
#'     x = c(FALSE, TRUE),
#'     cause = "Element 1 is false."
#' )
#' print(x)
goalie <- function(x, cause = character()) {
    stopifnot(
        is.logical(x),
        is.character(cause) && length(cause) <= 1L
    )
    new(Class = "goalie", x)
}
