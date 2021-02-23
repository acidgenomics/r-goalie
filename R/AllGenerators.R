#' Define a goalie check classed return
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @param object `logical`.
#' @param cause `character`.
#'   Corresponding cause attribute for logical `object`.
#'   Note that `TRUE` values must contain `NA_character_` cause.
#'   This value should not be named.
#'
#' @return `goalie`, which extends `logical`.
#'
#' @examples
#' x <- goalie(
#'     object = c("aaa" = FALSE, "bbb" = TRUE),
#'     cause = c(
#'         "'aaa' is FALSE.",
#'         NA_character_
#'     )
#' )
#' print(x)
#' print(cause(x))
goalie <- function(object, cause) {
    stopifnot(
        is.logical(object),
        length(object) >= 1L
    )
    if (isTRUE(all(object))) {
        stopifnot(isTRUE(missing(cause)))
        cause <- rep(x = NA_character_, times = length(object))
    } else {
        stopifnot(is.character(cause))
    }
    new(Class = "goalie", ".Data" = object, "cause" = cause)
}
