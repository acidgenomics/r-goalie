#' Define a goalie check classed return
#'
#' @export
#' @note Updated 2023-09-29.
#'
#' @param object `logical`.
#'
#' @param cause `character`.
#' Corresponding cause attribute for logical `object`.
#' Note that `TRUE` values must contain `NA_character_` cause.
#' This value should not be named.
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
    if (isTRUE(all(object))) {
        cause <- rep(x = NA_character_, times = length(object))
    }
    new(Class = "goalie", ".Data" = object, "cause" = cause)
}
